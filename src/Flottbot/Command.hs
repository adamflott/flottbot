module Flottbot.Command where

import           Flottbot.Prelude

-- base

-- hackage
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Map.Strict               as Map
import           Data.YAML
import           System.FilePath.Find


data Command = Command {
    _commandName          :: !Text
    , _commandEnabled     :: !Bool
    , _commandDescription :: !Text
    , _commandOwnerEmail  :: !Text
    , _commandUsage       :: !Text
    , _commandExec        :: !Text
    , _commandArgs        :: ![Text]
    } deriving (Eq, Show)

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
            .:  "exec"
            <*> m
            .:  "args"

type Commands = Map Text Command

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
