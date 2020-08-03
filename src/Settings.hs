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
defaultSettings = FormState { _hints=False, _controls=True, _escapeCode=False, _maxRecents=5}

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
    [ label "Draw hints using underscores for definition cards" @@= yesnoField hints HintsField ""
    , label "Show controls at the bottom of screen" @@= yesnoField controls ControlsField ""
    , label "Use the '-n \\e[5 q' escape code to change the cursor to a blinking line on start" @@= yesnoField escapeCode EscapeCodeField ""
    , label "Maximum number of recently selected files stored" @@= hLimit 3 @@= naturalNumberField maxRecents MaxRecentsField "" ]

yesnoField :: (Ord n, Show n) => Lens' s Bool -> n -> T.Text -> s -> FormFieldState s e n
yesnoField stLens name label initialState =
  let initVal = initialState ^. stLens

      handleEvent (MouseDown n _ _ _) s | n == name = return $ not s
      handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) s  = return $ not s
      handleEvent (VtyEvent (V.EvKey V.KEnter [])) s  = return $ not s
      handleEvent _ s = return s
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderYesno label name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderYesno :: T.Text -> n -> Bool -> Bool -> Widget n
renderYesno label n foc val =
  let addAttr = if foc then withDefAttr focusedFormInputAttr else id
  in clickable n $ (if val then addAttr (txt "Yes") else addAttr (txt "No") <+> txt " ") <+> txt label

naturalNumberField :: (Ord n, Show n) => Lens' s Int -> n -> T.Text -> s -> FormFieldState s e n
naturalNumberField stLens name label initialState =
  let initVal = initialState ^. stLens
      -- clamp s = 

      handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) s | isDigit c = return $ if s < 100 then read (show s ++ [c]) else s
      handleEvent (VtyEvent (V.EvKey V.KBS [])) s = return $ case show s of
                                                           "" -> 0
                                                           xs -> fromMaybe 0 (readMaybe (init xs))
      handleEvent _ s = return s
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderNaturalNumber label name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderNaturalNumber :: T.Text -> n -> Bool -> Int -> Widget n
renderNaturalNumber label n foc val =
  let addAttr = if foc then withDefAttr focusedFormInputAttr else id
      val' = show val
      csr = if foc then showCursor n (Location (length val',0)) else id
  in csr (addAttr (str val')) <+> txt label <+> hFill ' '
