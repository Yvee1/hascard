module UI.Info (State, drawUI, handleEvent, theMap, runInfoUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (void)
import States
import qualified Graphics.Vty as V

drawUI :: State -> [Widget Name]
drawUI = (:[]) . const ui

ui :: Widget Name
ui =
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimit 40 $
  hCenter (withAttr titleAttr (str "Info")) <=>
  hBorder <=>
  drawInfo

handleEvent :: GlobalState -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent s (VtyEvent e) =
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt s
      V.EvKey V.KEsc [] -> halt s
      V.EvKey V.KEnter [] -> halt s
      V.EvKey V.KDown [] -> vScrollBy (viewportScroll ()) 1 >> continue s
      V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll ()) 1 >> continue s
      V.EvKey V.KUp [] -> vScrollBy (viewportScroll ()) (-1) >> continue s
      V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll ()) (-1) >> continue s
      _ -> continue s
handleEvent s _ = continue s

titleAttr :: AttrName
titleAttr = attrName "title"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow) ]

drawInfo :: Widget Name
drawInfo = 
  padLeftRight 1 $
  vLimitPercent 60 $
  viewport () Vertical (strWrap info)

runInfoUI :: GlobalState -> GlobalState
runInfoUI = goToState (InfoState ())

info :: String
info = 
  "Hascard is a text-based user interface for reviewing notes using 'flashcards'. Cards are written in markdown-like syntax; for more info see the README file. Use the --help flag for information on the command line options.\n\nControls:\n * Use arrows or the j and k keys for menu navigation\n * Enter confirms a selection, flips a card or continues to the next card\n * Use TAB or the arrow keys for navigating gaps in open questions\n * Use the c key for confirming reorder questions or multiple choice questions with more than 1 possible answer\n * Use F1 to show the answers of a open question.\n * Use CTRL+Left and CTRL+Right to move to previous and next cards without having to answer them"