module UI.Info (runInfoUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (void)
import qualified Graphics.Vty as V

type Event = ()
type Name = ()
type State = ()

app :: App State Event Name
app = App 
  { appDraw = (:[]) . const ui
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

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

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
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

runInfoUI :: IO ()
runInfoUI = void $ defaultMain app ()

info :: String
info = 
  "Hascard is a text-based user interface for reviewing notes. Cards are written in markdown-like syntax; for more info see cards/syntax.txt in the git repository. Use the --help flag for information on the command line options.\n\nControls:\n * Use arrows or the j and k keys for menu navigation\n * Enter confirms a selection, flips a card or continues to the next card\n * Use TAB or the arrow keys for navigating gaps in open questions\n * Use the c key for confirming multiple choice questions with more than 1 possible answer\n * Use CTRL+Left and CTRL+Right to move to previous and next cards without having to answer them"