module Settings where
import States
import Data.Map.Strict ((!))
import Data.Functor (($>))
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified System.Directory as D
import qualified Data.Map.Strict as M

getShowHints :: IO Bool
getShowHints = do
  settings <- getSettings
  return $ settings ! 0 

getShowControls :: IO Bool
getShowControls = do
  settings <- getSettings
  return $ settings ! 1

getUseEscapeCode :: IO Bool
getUseEscapeCode = do
  settings <- getSettings
  return $ settings ! 2

getSettings :: IO Settings
getSettings = do
  sf <- getSettingsFile
  exists <- D.doesFileExist sf
  if exists 
    then do
      maybeSettings <- parseSettings <$> readFile sf
      flip (maybe (return defaultSettings)) maybeSettings $ \settings ->
        if M.size settings == M.size defaultSettings
          then return settings
          else let settings' = settings `mergeWithDefault` defaultSettings in
            setSettings settings' $> settings'

  else return defaultSettings

mergeWithDefault :: Settings -> Settings -> Settings
mergeWithDefault = flip M.union

parseSettings :: String -> Maybe Settings
parseSettings = readMaybe

getSettingsFile :: IO FilePath
getSettingsFile = do
  maybeSnap <- lookupEnv "SNAP_USER_DATA"
  xdg <- D.getXdgDirectory D.XdgConfig "hascard"

  let dir = case maybeSnap of
                Just path | not (null path) -> path
                          | otherwise       -> xdg
                Nothing                     -> xdg
  D.createDirectoryIfMissing True dir
  return (dir </> "settings")

defaultSettings :: Settings
defaultSettings = M.fromList [(0, False), (1, True), (2, False)]

setSettings :: Settings -> IO ()
setSettings settings = do
  sf <- getSettingsFile
  writeFile sf (show settings)