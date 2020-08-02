module UI.Settings (State, drawUI, handleEvent, theMap) where

import Brick hiding (mergeWithDefault)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Data.Map.Strict ((!))
import States
import Settings
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V

drawUI :: SS -> [Widget Name]
drawUI = (:[]) . ui

ui :: SS -> Widget Name
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

handleEvent :: GlobalState -> SS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs (i, settings) (VtyEvent e) =
  let update = updateSS gs
      halt' global = continue (popState global) <* liftIO (setSettings settings)  in
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> halt' gs
      V.EvKey V.KEsc [] -> halt' gs
      V.EvKey V.KEnter [] -> continue $ update (i, settings')
        where settings' = M.adjust not i settings
      V.EvKey V.KUp [] -> continue $ update (max 0 (i-1), settings)
      V.EvKey (V.KChar 'k') [] -> continue $ update (max 0 (i-1), settings)
      V.EvKey V.KDown [] -> continue $ update (min (M.size settings-1) (i+1), settings)
      V.EvKey (V.KChar 'j') [] -> continue $ update (min (M.size settings-1) (i+1), settings)
      _ -> continue gs
handleEvent gs _ _ = continue gs

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow),
      (selectedAttr, V.defAttr `V.withStyle` V.underline) ]

drawSettings :: SS -> Widget Name
drawSettings s = vBox $ map (drawSetting s) (zip [0..] descriptions)
  where descriptions = map (++": ") 
          [ "Draw hints using underscores for definition cards"
          , "Show controls at the bottom of screen"
          , "Use the '-n \\e[5 q' escape code to change the cursor to a blinking line on start" ]

drawSetting :: SS -> (Int, String) -> Widget Name
drawSetting (selected, settings) (i, text) =
  strWrap text <+> str "  " <+> word
  where word = if settings ! i then underline (str "Yes") else underline (str "No") <+> str " "
        underline = if i == selected then withAttr selectedAttr else id
