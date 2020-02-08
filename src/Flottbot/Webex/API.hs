module Flottbot.Webex.API where

import           Flottbot.Prelude

-- base
import           System.Exit
import           System.Timeout

-- Hackage

import           Chronos
import           Control.Concurrent.Chan.Unagi.Bounded
import           Data.Text
import           Network.HTTP.Client            ( Response
                                                , responseBody
                                                )
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.WebexTeams
import           Network.WebexTeams.Types
import           Servant
import           System.Directory
import           System.FilePath.Posix
import           System.Process.Typed
import           UnliftIO.Async
import qualified Data.Map.Strict               as Map

-- local
import           Flottbot.Command
import           Flottbot.App
import           Flottbot.Logging


-- HTTP API
type API = APIIndex :<|> APIEventNew
type APIIndex = Get '[PlainText] NoContent
type APIEventNew = ReqBody '[JSON] WebhookNotify :> PostNoContent '[JSON] NoContent


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
    ev <- liftIO $ readChan q
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

            case getCommand userCmd cmds of
                Nothing -> do
                    newLogEvent (LogEventCommandNotFound userCmd)
                    worker
                Just cmd -> do
                    cwd <- liftIO getCurrentDirectory

                    let fullCmdPath = toText (cwd </> "commands" </> toString userCmd </> toString (_commandExec cmd))
                        cmdArgs     = _commandArgs cmd ++ [userArgs]
                    (dur, cmdResp) <- liftIO $ stopwatch $ runCommand fullCmdPath cmdArgs (cfg ^. commandTimeoutInSeconds)

                    case cmdResp of
                        Left  (ec, err) -> newLogEvent (LogEventCommandRunResult fullCmdPath cmdArgs ec err dur)
                            -- TODO reply to channel command failed
                        Right out       -> do
                            newLogEvent (LogEventCommandRunResult fullCmdPath cmdArgs ExitSuccess out dur)
                            if Data.Text.length out > 0
                                then do
                                    let message =
                                            CreateMessage roomId Nothing Nothing (Just (MessageText out)) Nothing Nothing
                                    -- TODO http-client-0.6.4:Network.HTTP.Client.Types.Response Message’
                                    (apiDur, tr) <- liftIO $ stopwatch $ createEntity auth def message
                                    newLogEvent (LogEventWebexAPIResponse tr apiDur)

                                    pure ()
                                else pure ()


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
server :: AppContext -> Server API
server st = idx :<|> newEvent st

idx :: Handler NoContent
idx = pure NoContent

newEvent :: AppContext -> WebhookNotify -> Handler NoContent
newEvent ctx ev = do
    let q = ctx ^. appCtxWebex . webexCtxQueueInEv

    liftIO $ do
        logEvent (ctx ^. appCtxLog) (LogEventEnqueueWebexMessage ev)
        writeChan q ev

    return NoContent

-- App
evApp :: AppContext -> Application
evApp st = serve api (server st)

runWebexWebhookAPI :: App ()
runWebexWebhookAPI = do
    cfg    <- getCfg
    ctx    <- getCtx
    logCtx <- getLogCtx

    let logOpenConnection sa = logEvent logCtx (LogEventConenctionOpen sa)
        logCloseConnection sa = logEvent logCtx (LogEventConenctionClose sa)
        logRequest req status fileSize = logEvent logCtx (LogEventHTTPRequest req status fileSize)

    let warpSettings =
            defaultSettings
                & setHost (fromString (toString (cfg ^. webexCfgWebhookListen)))
                & setTimeout 10 -- TODO add to config
                & setPort (fromIntegral (cfg ^. webexCfgWebhookPort))
                & setLogger logRequest
                & setOnOpen (\sa -> logOpenConnection sa >> pure True)
                & setOnClose logCloseConnection
                & setBeforeMainLoop (logEvent logCtx (LogEventOp OpStarted))
                & setServerName appName
                & setGracefulShutdownTimeout (Just 10) -- TODO add to config

    ws <- replicateM (fromIntegral (cfg ^. webexCfgWebhookEventWorkerCount)) (async worker)

    liftIO $ runSettings warpSettings (evApp ctx)

    mapM_ wait ws

-- Servant
api :: Proxy API
api = Proxy
