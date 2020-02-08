module Flottbot.Logging where


import           Flottbot.Prelude

-- base
import           System.Exit

-- hackage
import           Chronos
import           Control.Concurrent.Chan.Unagi.Bounded
import           Network.HTTP.Client     hiding ( Request )
import           Network.HTTP.Types.Status
import           Network.Socket
import           Network.Wai             hiding ( Response )
import           Network.WebexTeams.Types
import           UnliftIO                hiding ( newChan
                                                , writeChan
                                                , readChan
                                                )
import           UnliftIO.Concurrent            ( ThreadId
                                                , myThreadId
                                                , forkIO
                                                )

-- local
import           Flottbot.Config


data Operation
    = OpStarted
    | OpStopping
    | OpStopped
    deriving Show

data LogEvent
    = LogEventOp !Operation
    | LogEventConfig !Config
    | LogEventConenctionOpen !SockAddr
    | LogEventConenctionClose !SockAddr
    | LogEventEnqueueWebexMessage !WebhookNotify
    | LogEventDequeueWebexMessage !WebhookNotify
    | LogEventCommandNotFound !Text
    | LogEventCommandRunResult !Text ![Text] ExitCode !Text !Timespan
    | LogEventHTTPRequest !Request !Status !(Maybe Integer)
    | LogEventWebexAPIResponse !(Response Message) !Timespan
    deriving Show

data LogEventWithDetails = LogEventWithDetails {
    _ts    :: Time
    , _tid :: ThreadId
    , _lev :: LogEvent
    }
  deriving Show

data LoggingContext = LoggingContext {
    _loggingCfg        :: Config
    , _loggingCtxInEv  :: InChan LogEventWithDetails
    , _loggingCtxOutEv :: OutChan LogEventWithDetails
    }

makeLenses ''LoggingContext

type LoggerState = MonadReader LoggingContext


newtype Logger a = Logger {
    runLogger :: ReaderT LoggingContext IO a
} deriving newtype (Monad, Functor, Applicative, LoggerState, MonadIO, MonadUnliftIO)


newLoggingCtx :: Config -> IO LoggingContext
newLoggingCtx cfg = do
    (cin, cout) <- newChan (fromIntegral (cfg ^. loggingQueueSize))

    pure $ LoggingContext cfg cin cout

logEvent :: LoggingContext -> LogEvent -> IO ()
logEvent ctx ev = do
    let q = _loggingCtxInEv ctx

    ts  <- now
    tid <- myThreadId

    writeChan q (LogEventWithDetails ts tid ev)

startLogger :: LoggingContext -> IO ()
startLogger ctx = do
    _ <- forkIO (loggerWorker ctx)
    pure ()

loggerWorker :: LoggingContext -> IO ()
loggerWorker ctx = do
    ev <- readChan (ctx ^. loggingCtxOutEv)
    let enabled = ctx ^. loggingCfg . loggingEnabled

    if enabled
        then do
            print ev
        else pure ()

    loggerWorker ctx
