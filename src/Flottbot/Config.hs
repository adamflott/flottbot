module Flottbot.Config where

import           Flottbot.Prelude

-- hackage
import           Data.YAML

data Config = Config {
    _loggingEnabled                        :: !Bool
    , _loggingQueueSize                    :: !Word32
    , _commandTimeoutInSeconds             :: !Word32
    , _webexCfgWebhookListen               :: !Text
    , _webexCfgWebhookPort                 :: !Word32
    , _webexCfgWebhookEventWorkerCount     :: !Word32
    , _webexCfgWebhookEventWorkerQueueSize :: !Word16
    , _webexCfgAccessToken                 :: !Text
    , _webexCfgWebhookId                   :: !Text
    , _webexCfgBotId                       :: !Text
    , _webexCfgBotName                     :: !Text
    , _webexCfgUserName                    :: !Text
    } deriving Show

makeLenses ''Config

instance FromYAML Config where
    parseYAML = withMap "flottbot config" $ \m ->
        Config
            <$> m
            .:  "loggingEnabled"
            <*> m
            .:  "loggingQueueSize"
            <*> m
            .:  "commandTimeoutInSeconds"
            <*> m
            .:  "webexWebhookListen"
            <*> m
            .:  "webexWebhookPort"
            <*> m
            .:  "webexWebhookEventWorkerCount"
            <*> m
            .:  "webexWebhookEventWorkerQueueSize"
            <*> m
            .:  "webexAccessToken"
            <*> m
            .:  "webexWebhookId"
            <*> m
            .:  "webexBotId"
            <*> m
            .:  "webexBotName"
            <*> m
            .:  "webexBotUserName"
