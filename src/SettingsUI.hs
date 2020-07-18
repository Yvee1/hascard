module SettingsUI (runSettingsUI, getShowHints, getShowControls) where

import Brick
import BrickHelpers
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (void)
import Data.Functor (($>))
import Data.Map.Strict (Map, (!))
import System.FilePath ((</>))
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import qualified System.Directory as D

type Event = ()
type Name = ()
type Settings = Map Int Bool
type State = (Int, Settings)

app :: App State Event Name
app = App 
  { appDraw = (:[]) . ui
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

ui :: State -> Widget Name
ui s =
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hLimit 40 $
  hCenter (withAttr titleAttr (str "Settings")) <=>
  hBorder <=>
  padLeftRight 1
  (drawSettings s)

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@(i, settings) (VtyEvent e) =
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> halt s
      V.EvKey V.KEsc [] -> halt s
      V.EvKey V.KEnter [] -> continue (i, settings')
        where settings' = M.adjust not i settings
      V.EvKey V.KUp [] -> continue (max 0 (i-1), settings)
      V.EvKey (V.KChar 'k') [] -> continue (max 0 (i-1), settings)
      V.EvKey V.KDown [] -> continue (min (M.size settings-1) (i+1), settings)
      V.EvKey (V.KChar 'j') [] -> continue (min (M.size settings-1) (i+1), settings)
      _ -> continue s
handleEvent s _ = continue s

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow),
      (selectedAttr, V.defAttr `V.withStyle` V.underline) ]

drawSettings :: State -> Widget Name
drawSettings s = vBox $ map (drawSetting s) (zip [0..] descriptions)
  where descriptions = map (++": ") 
          [ "Draw hints using underscores for definition cards"
          , "Show controls at the bottom of screen" ]

drawSetting :: State -> (Int, String) -> Widget Name
drawSetting (selected, settings) (i, text) =
  strWrap text <+> str " " <+> word
  where word = if settings ! i then underline (str "Yes") else underline (str "No") <+> str " "
        underline = if i == selected then withAttr selectedAttr else id

runSettingsUI :: IO ()
runSettingsUI = do
  currentSettings <- getSettings
  (_, newSettings) <- defaultMain app (0, currentSettings)
  setSettings newSettings

getSettings :: IO Settings
getSettings = do
  sf <- getSettingsFile
  exists <- D.doesFileExist sf
  if exists
    then parseSettings <$> readFile sf
    else return defaultSettings

getShowHints :: IO Bool
getShowHints = do
  settings <- getSettings
  return $ settings ! 0 

getShowControls :: IO Bool
getShowControls = do
  settings <- getSettings
  return $ settings ! 1

parseSettings :: String -> Settings
parseSettings = read

getSettingsFile :: IO FilePath
getSettingsFile = do
  xdg <- D.getXdgDirectory D.XdgConfig "hascard"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "settings")

defaultSettings :: Settings
defaultSettings = M.fromList [(0, True), (1, True)]

setSettings :: Settings -> IO ()
setSettings settings = do
  sf <- getSettingsFile
  writeFile sf (show settings)