module Flottbot.Commands where

import           Flottbot.Prelude

-- hackage
import qualified Data.Map.Strict               as Map
import           Data.Text

-- local
import           Flottbot.Command

internalCmds :: Commands -> Commands
internalCmds c = Map.union c (Map.fromList cmdsToTuples)

cmdsToTuples :: [(Text, Command)]
cmdsToTuples = fmap cmdToTuple [cmdEnabled, cmdHelp]

cmdToTuple :: Command -> (Text, Command)
cmdToTuple cmd = (_commandName cmd, cmd)

amfEmail :: IsString p => p
amfEmail = "adam@adamflott.com"

-- Commands --------------------------------------------------------------------

cmdHelp :: Command
cmdHelp = Command { _commandName        = "help"
                  , _commandEnabled     = True
                  , _commandDescription = "Display help for a given command"
                  , _commandOwnerEmail  = amfEmail
                  , _commandUsage       = "!help"
                  , _commandType        = CommandTypeInternal (CommandTypeInternalHandlerPure cmdHelpHandler)
                  }

cmdHelpHandler :: (Text, Text, Commands) -> Either Text Text
cmdHelpHandler (cmdName, cmdArg, allCmds) = case Map.lookup cmdArg allCmds of
    Nothing  -> Left $ "Command '" <> cmdName <> "' not found"
    Just cmd -> Right (cmd ^. commandDescription)

--

cmdEnabled :: Command
cmdEnabled = Command { _commandName        = "cmds-enabled"
                     , _commandEnabled     = True
                     , _commandDescription = "List currently enabled commands"
                     , _commandOwnerEmail  = amfEmail
                     , _commandUsage       = "!cmds-enabled"
                     , _commandType        = CommandTypeInternal (CommandTypeInternalHandlerPure cmdEnabledHandler)
                     }

cmdEnabledHandler :: (Text, Text, Commands) -> Either Text Text
cmdEnabledHandler (_, _, allCmds) = do
    let (out, _) = Map.mapAccumWithKey buildEnabledCommands [] allCmds

    Right (Data.Text.intercalate ", " out)
  where
    buildEnabledCommands :: [Text] -> Text -> Command -> ([Text], Command)
    buildEnabledCommands outText cmdName cmd = if cmd ^. commandEnabled then (outText ++ [cmdName], cmd) else (outText, cmd)
