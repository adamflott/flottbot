module Main where

import           Flottbot.Prelude

-- base
import           Data.Version                   ( showVersion )

-- Hackage
import           Data.YAML                     as YAML
import           Options.Applicative           as Opt

-- local
import           Flottbot.App
import           Flottbot.Command
import           Flottbot.Commands
import           Flottbot.Logging
import           Flottbot.Webex.API


-- local (via cabal)
import           Paths_flottbot                 ( version )


main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Args -> IO ()
runProgram (     Args _                  True ) = putStrLn (showVersion Paths_flottbot.version)
runProgram (     Args Nothing            _    ) = putStrLn "A config file is required" >> exitFailure
runProgram args@(Args (Just cfgFilePath) False) = do
    ecfg   <- configLoad cfgFilePath
    cfg    <- checkCfgOrExit ecfg

    ecmds  <- commandsLoad
    cmds   <- checkCommandsOrExit ecmds
    let allCmds = cmds <> internalCmds cmds


    logCtx <- newLoggingCtx cfg
    startLogger logCtx

    mapM_ (logEvent logCtx . LogEventCommandNew) allCmds

    appCtx <- newAppCtx args cfg allCmds logCtx

    logEvent logCtx (LogEventConfig cfg)

    runReaderT (runApp runWebexWebhookAPI) appCtx

checkCfgOrExit :: Either (Pos, String) Config -> IO Config
checkCfgOrExit c = case c of
    Left  err -> print err >> exitFailure
    Right cfg -> pure cfg

checkCommandsOrExit :: Either [String] Commands -> IO Commands
checkCommandsOrExit c = case c of
    Left  err  -> print err >> exitFailure
    Right cmds -> pure cmds

parseCLI :: IO Args
parseCLI = execParser (withInfo parseArgs appName)
  where
    withInfo opts h = info
        (helper <*> opts)
        (header (h <> " - a bot for Cisco Webex Teams") <> fullDesc <> progDesc "Cisco Webex Teams bot")


parseArgs :: Opt.Parser Args
parseArgs = Args <$> configFileArgParser <*> versionArgParser

configFileArgParser :: Opt.Parser (Maybe FilePath)
configFileArgParser = optional
    (strOption
        (long "config-file" <> short 'c' <> metavar "FILE" <> value appConfigFilename <> showDefault <> help
            "YAML config file path"
        )
    )

versionArgParser :: Opt.Parser Bool
versionArgParser = switch (long "version" <> short 'v' <> help "Print version")
