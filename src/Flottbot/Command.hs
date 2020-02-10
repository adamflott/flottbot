module Flottbot.Command where

import           Flottbot.Prelude

-- base
import           Text.Show

-- hackage
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Map.Strict               as Map
import           Data.YAML
import           System.FilePath.Find

type Commands = Map Text Command

data CommandTypeInternalHandler
    = CommandTypeInternalHandlerIO   ((Text, Text, Commands) -> IO (Either Text Text))
    | CommandTypeInternalHandlerPure ((Text, Text, Commands) -> Either Text Text)

data CommandType
    = CommandTypeExec {
        _commandExec   :: !Text
        , _commandArgs :: ![Text]
        }
    | CommandTypeInternal !CommandTypeInternalHandler
    | CommandTypeUnknown

instance Show CommandType where
    show (CommandTypeExec e a  ) = Text.Show.show e <> " " <> Text.Show.show a
    show (CommandTypeInternal _) = "internal"
    show CommandTypeUnknown      = "unknonwn"

instance FromYAML CommandType where
    parseYAML = withMap "flottbot command type" $ \m -> do
        t <- m .: "type"

        case t :: Text of
            "executable" -> do
                e <- m .: "exec"
                a <- m .: "args"
                pure $ CommandTypeExec e a
            _ -> pure CommandTypeUnknown

data Command = Command {
    _commandName          :: !Text
    , _commandEnabled     :: !Bool
    , _commandDescription :: !Text
    , _commandOwnerEmail  :: !Text
    , _commandUsage       :: !Text
    , _commandType        :: !CommandType
    } deriving (Show)

makeLenses ''Command

instance FromYAML Command where
    parseYAML = withMap "flottbot command" $ \m ->
        Command
            <$> m
            .:  "commandName"
            <*> m
            .:  "enabled"
            <*> m
            .:  "description"
            <*> m
            .:  "ownerEmail"
            <*> m
            .:  "usage"
            <*> m
            .:  "command"

search :: String -> FilePath -> IO [FilePath]
search pat = System.FilePath.Find.find always (fileName ~~? pat)

getAllIndexes :: IO [FilePath]
getAllIndexes = search "*.yaml" "./commands"

readAllIndexes :: [FilePath] -> IO [Either (Pos, String) Command]
readAllIndexes = mapM readAndDecodeYAML
  where
    readAndDecodeYAML file = do
        idxRaw <- BSC.readFile file
        pure $ decode1Strict idxRaw

loadIndexes :: IO ([(Pos, String)], [Command])
loadIndexes = do
    idxFilepaths <- getAllIndexes
    fmap partitionEithers (readAllIndexes idxFilepaths)

listToMap :: [Command] -> Commands -> Commands
listToMap cmds m = foldl' (\cm cmd -> Map.insert (_commandName cmd) cmd cm) m cmds
