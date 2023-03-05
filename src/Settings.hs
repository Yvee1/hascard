{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Settings where
import Brick
import Brick.Forms
import UI.BrickHelpers
import Data.Char (isDigit)
import States
import Data.Maybe
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Lens.Micro.Platform
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified System.Directory as D

getShowHints :: IO Bool
getShowHints = do
  settings <- getSettings
  return $ settings ^. hints

getShowControls :: IO Bool
getShowControls = do
  settings <- getSettings
  return $ settings ^. controls

getCaseSensitive :: IO Bool
getCaseSensitive = do
  settings <- getSettings
  return $ settings ^. caseSensitive

getShuffleAnswers :: IO Bool
getShuffleAnswers = do
  settings <- getSettings
  return $ settings ^. shuffleAnswers

getUseEscapeCode :: IO Bool
getUseEscapeCode = do
  settings <- getSettings
  return $ settings ^. escapeCode

getMaxRecents :: IO Int
getMaxRecents = do
  settings <- getSettings
  return $ settings ^. maxRecents

getSettings :: IO Settings
getSettings = do
  sf <- getSettingsFile
  exists <- D.doesFileExist sf
  if exists 
    then do
      maybeSettings <- parseSettings <$> readFile sf
      maybe (return defaultSettings) return maybeSettings
  else return defaultSettings

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
defaultSettings = FormState { _hints=False, _controls=True, _caseSensitive=True, 
  _shuffleAnswers=False, _escapeCode=False, _maxRecents=5}

setSettings :: Settings -> IO ()
setSettings settings = do
  sf <- getSettingsFile
  writeFile sf (show settings)

settingsState :: IO State
settingsState = SettingsState . mkForm <$> getSettings

mkForm :: Settings -> Form Settings e Name
mkForm =
  let label s w = padBottom (Pad 1) $ padRight (Pad 2) (strWrap s) <+> w
  
  in newForm
    [ label "Draw hints using underscores for definition cards" @@= yesnoField False hints HintsField ""
    , label "Show controls at the bottom of screen" @@= yesnoField False controls ControlsField ""
    , label "Open questions are case sensitive" @@= yesnoField False caseSensitive CaseSensitiveField ""
    , label "Shuffle answers to multiple choice questions" @@= yesnoField False shuffleAnswers ShuffleAnswersField ""
    , label "Use the '-n \\e[5 q' escape code to change the cursor to a blinking line on start" @@= yesnoField False escapeCode EscapeCodeField ""
    , label "Maximum number of recently selected files stored" @@= naturalNumberField 999 maxRecents MaxRecentsField "" ]

