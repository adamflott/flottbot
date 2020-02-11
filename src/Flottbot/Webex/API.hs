module Flottbot.Webex.API where

import           Flottbot.Prelude

-- base
import           System.Exit
import           System.Timeout

-- Hackage

import           Chronos
import           Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import           Data.Text
import           Network.HTTP.Client            ( Response
                                                , responseBody
                                                )
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.Gzip
import           Network.WebexTeams
import           Network.WebexTeams.Types
import           Servant
import           System.Directory
import           System.FilePath.Posix
import           System.Process.Typed
import           UnliftIO.STM
import           UnliftIO.Async
import qualified Data.Map.Strict               as Map

-- local
import           Flottbot.Command
import           Flottbot.App
import           Flottbot.Logging


-- HTTP API
type API = APIIndex :<|> APIEventNew :<|> APIShutdown

type APIIndex = Get '[PlainText] NoContent
type APIEventNew = ReqBody '[JSON] WebhookNotify :> PostNoContent '[JSON] NoContent
type APIShutdown = "shutdown" :> Get '[PlainText] NoContent

webexDomain :: IsString a => a
webexDomain = "webex.bot"

worker :: App ()
worker = do
    cfg      <- getCfg
    cmds     <- getCmds
    webexCtx <- getWebexCtx

    let q                    = webexCtx ^. webexCtxQueueOutEv
        fullBotName          = cfg ^. webexCfgBotName <> "@" <> webexDomain
        continueIfNotFromBot = maybe worker (\(Email e) -> if e == fullBotName then worker else pure ())

    -- blocks until an item is available
    ev <- liftIO $ Chan.readChan q
    newLogEvent (LogEventDequeueWebexMessage ev)

    continueIfNotFromBot (getFrom ev)

    case getMessageId ev of
        Nothing -> worker
        Just m  -> do
            handleMsg cfg cmds ev m
            worker

newLogEvent :: LogEvent -> App ()
newLogEvent ev = do
    logCtx <- getLogCtx
    liftIO $ logEvent logCtx ev

handleMsg :: (WebexTeamsDetail key, ToResponse key ~ Message) => Config -> Commands -> WebhookNotify -> key -> App ()
handleMsg cfg cmds ev m = do
    let auth   = Authorization (encodeUtf8 (cfg ^. webexCfgAccessToken))
        roomId = getRoomId ev

    r <- liftIO $ getDetail auth def m

    case getMessage r of
        Nothing               -> worker
        Just (MessageText mt) -> do

            -- remove '!' and split at first space
            let (userCmd, userArgs) = Data.Text.breakOn " " (Data.Text.drop 1 mt)
                userArgsCleaned     = Data.Text.strip userArgs

            case getCommand userCmd cmds of
                Nothing -> do
                    newLogEvent (LogEventCommandNotFound userCmd)
                    worker
                Just cmd -> do
                    cwd <- liftIO getCurrentDirectory

                    case cmd ^. commandType of
                        (CommandTypeExec e a) -> do
                            let fullCmdPath = toText (cwd </> "commands" </> toString userCmd </> toString e)
                                cmdArgs     = a ++ [userArgsCleaned]
                            (dur, cmdResp) <- liftIO $ stopwatch $ runCommand fullCmdPath
                                                                              cmdArgs
                                                                              (cfg ^. commandTimeoutInSeconds)

                            case cmdResp of
                                Left  (ec, err) -> newLogEvent (LogEventCommandRunResult fullCmdPath cmdArgs ec err dur)
                                -- TODO reply to channel command failed
                                Right out       -> do
                                    newLogEvent (LogEventCommandRunResult fullCmdPath cmdArgs ExitSuccess out dur)
                                    if Data.Text.length out > 0
                                        then do
                                            let
                                                message = CreateMessage roomId
                                                                        Nothing
                                                                        Nothing
                                                                        (Just (MessageText out))
                                                                        Nothing
                                                                        Nothing
                                            -- TODO http-client-0.6.4:Network.HTTP.Client.Types.Response Message’
                                            (apiDur, tr) <- liftIO $ stopwatch $ createEntity auth def message
                                            newLogEvent (LogEventWebexAPIResponse tr apiDur)

                                            pure ()
                                        else pure ()
                        (CommandTypeInternal h) -> case h of
                            CommandTypeInternalHandlerIO h' -> do
                                _ <- liftIO $ h' (userCmd, userArgsCleaned, cmds)
                                pure ()
                            CommandTypeInternalHandlerPure h' -> do
                                let (Right out) = h' (userCmd, userArgsCleaned, cmds)

                                let message = CreateMessage roomId Nothing Nothing (Just (MessageText out)) Nothing Nothing
                                -- TODO http-client-0.6.4:Network.HTTP.Client.Types.Response Message’
                                (apiDur, tr) <- liftIO $ stopwatch $ createEntity auth def message
                                newLogEvent (LogEventWebexAPIResponse tr apiDur)

                                pure ()

                        CommandTypeUnknown -> pure ()


getCommand :: Text -> Commands -> Maybe Command
getCommand = Map.lookup

runCommand :: Text -> [Text] -> Word32 -> IO (Either (ExitCode, Text) Text)
runCommand cmd args tm = do

    let oneSecond = 1000000
    r <- timeout (fromIntegral tm * oneSecond) $ readProcess (proc (toString cmd) (fmap toString args))

    case r of
        Nothing                   -> pure $ Left (ExitFailure 30, "command timed out after " <> show tm <> " seconds")
        Just (exitCode, out, err) -> case exitCode of
            ExitFailure _ -> pure $ Left (exitCode, decodeUtf8 err)
            ExitSuccess   -> pure $ Right (decodeUtf8 out)


getFrom :: WebhookNotify -> Maybe Email
getFrom ev = case webhookNotifyData ev of
    WebhookNotifyDataMsg m -> messagePersonEmail m
    _                      -> Nothing

getMessageId :: WebhookNotify -> Maybe MessageId
getMessageId ev = case webhookNotifyData ev of
    WebhookNotifyDataMsg m -> Just (messageId m)
    _                      -> Nothing



getMessage :: Response Message -> Maybe MessageText
getMessage = messageText . responseBody


isMyMessage :: WebhookNotify -> Bool
isMyMessage _ = True

getRoomId :: WebhookNotify -> Maybe RoomId
getRoomId ev = case webhookNotifyData ev of
    WebhookNotifyDataMsg        m                           -> messageRoomId m
    WebhookNotifyDataRoom       (WebhookNotifyRoom       r) -> Just (roomId r)
    WebhookNotifyDataMembership (WebhookNotifyMembership m) -> membershipRoomId m



-- Handlers
server :: AppContext -> TMVar () -> Server API
server st t = wwwIdx :<|> wwwNewEvent st :<|> wwwShutdown t

wwwIdx :: Handler NoContent
wwwIdx = pure NoContent

wwwNewEvent :: AppContext -> WebhookNotify -> Handler NoContent
wwwNewEvent ctx ev = do
    let q = ctx ^. appCtxWebex . webexCtxQueueInEv

    liftIO $ do
        logEvent (ctx ^. appCtxLog) (LogEventEnqueueWebexMessage ev)
        Chan.writeChan q ev

    pure NoContent

wwwShutdown :: TMVar () -> Handler NoContent
wwwShutdown t = do
    shouldRun <- UnliftIO.STM.atomically $ isEmptyTMVar t
    if shouldRun then UnliftIO.STM.atomically $ putTMVar t () else throwError err503
    pure NoContent

-- App
evApp :: AppContext -> TMVar () -> Application
evApp ctx t = serve api (server ctx t)

runWebexWebhookAPI :: TMVar () -> TVar Int -> App ()
runWebexWebhookAPI shutdown activeConnections = do
    cfg    <- getCfg
    ctx    <- getCtx
    logCtx <- getLogCtx

    let logRequest req status fileSize = logEvent logCtx (LogEventHTTPRequest req status fileSize)

    let onOpen sa = do
            UnliftIO.STM.atomically $ modifyTVar' activeConnections (+ 1)
            logEvent logCtx (LogEventConenctionOpen sa)
            pure True

    let onClose sa = do
            logEvent logCtx (LogEventConenctionClose sa)
            UnliftIO.STM.atomically $ modifyTVar' activeConnections (subtract 1)

    let warpSettings =
            defaultSettings
                & setHost (fromString (toString (cfg ^. webexCfgWebhookListen)))
                & setTimeout 10 -- TODO add to config
                & setPort (fromIntegral (cfg ^. webexCfgWebhookPort))
                & setLogger logRequest
                & setOnOpen onOpen
                & setOnClose onClose
                & setBeforeMainLoop (logEvent logCtx (LogEventOp OpStarted))
                & setServerName appName
                & setGracefulShutdownTimeout (Just 10) -- TODO add to config

    ws <- replicateM (fromIntegral (cfg ^. webexCfgWebhookEventWorkerCount)) (async worker)

    let tlsCerts = tlsSettings (toString (cfg ^. tlsCertFilePath)) (toString (cfg ^. tlsKeyFilePath))
    liftIO $ runTLS tlsCerts warpSettings (evApp ctx shutdown)

    mapM_ wait ws


-- Servant
api :: Proxy API
api = Proxy
