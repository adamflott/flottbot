module Flottbot.App
    ( appName
    , appConfigFilename
    , tlsCertFilePath
    , tlsKeyFilePath
    , Args(..)
    , argsConfigFilePath
    , argsVersion
    , getArgs
    , AppContext
    , commandTimeoutInSeconds
    , Config(..)
    , webexCfgWebhookListen
    , webexCfgWebhookPort
    , webexCfgWebhookEventWorkerCount
    , webexCfgWebhookEventWorkerQueueSize
    , webexCfgAccessToken
    , webexCfgWebhookId
    , webexCfgBotId
    , webexCfgBotName
    , webexCfgUserName
    , configLoad
    , getCfg
    , App(..)
    , commandsLoad
    , getCmds
    , newAppCtx
    , AppState
    , getCtx
    , getWebexCtx
    , webexCtxQueueInEv
    , webexCtxQueueOutEv
    , appCtxWebex
    , appCtxLog
    , getLogCtx
    )
where

import           Flottbot.Prelude

-- base

-- hackage
import           Control.Concurrent.Chan.Unagi.Bounded
import qualified Data.ByteString.Char8         as BSC
import           Data.YAML
import           Network.WebexTeams.Types
import           UnliftIO                hiding ( newChan )

-- local
import           Flottbot.Command
import           Flottbot.Config
import           Flottbot.Logging


data Args = Args {
    _argsConfigFilePath :: !(Maybe FilePath)
    , _argsVersion :: !Bool
    } deriving Show

makeLenses ''Args


data WebexContext = WebexContext {
   _webexCtxQueueInEv  :: InChan WebhookNotify
  , _webexCtxQueueOutEv :: OutChan WebhookNotify
}

makeLenses ''WebexContext

data AppContext = AppContext {
  _appCtxArgs   :: !Args
  , _appCtxCfg  :: !Config
  , _appCtxCmds :: !Commands
  , _appCtxLog  :: !LoggingContext
  , _appCtxWebex :: !WebexContext
  }

makeLenses ''AppContext


type AppState = MonadReader AppContext

newtype App a = App {
    runApp :: ReaderT AppContext IO a
} deriving newtype (Monad, Functor, Applicative, AppState, MonadIO, MonadUnliftIO)


appName :: IsString a => a
appName = "flottbot"

appConfigFilename :: (Semigroup a, IsString a) => a
appConfigFilename = appName <> ".yaml"

--

getArgs :: App Args
getArgs = asks _appCtxArgs

--

configLoad :: FilePath -> IO (Either (Pos, String) Config)
configLoad cfgFilePath = fmap decode1Strict (BSC.readFile cfgFilePath) -- TODO use a safer readFile

getCfg :: App Config
getCfg = asks _appCtxCfg

--

newAppCtx :: Args -> Config -> Commands -> LoggingContext -> IO AppContext
newAppCtx args cfg cmds logCtx = do
    wctx <- newWebexCtx cfg
    pure $ AppContext args cfg cmds logCtx wctx

getCtx :: App AppContext
getCtx = ask

--

commandsLoad :: IO (Either [String] Commands)
commandsLoad = do
    (errs, _cmds) <- liftIO loadIndexes

    pure $ bool (Left (map snd errs)) (Right (listToMap _cmds mempty)) (null errs)

getCmds :: App Commands
getCmds = asks _appCtxCmds

--

newWebexCtx :: Config -> IO WebexContext
newWebexCtx cfg = do
    (cin, cout) <- newChan (fromIntegral (cfg ^. webexCfgWebhookEventWorkerQueueSize))
    pure $ WebexContext cin cout

getWebexCtx :: App WebexContext
getWebexCtx = asks _appCtxWebex

--

getLogCtx :: App LoggingContext
getLogCtx = asks _appCtxLog
