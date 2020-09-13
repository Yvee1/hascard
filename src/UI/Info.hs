module UI.Info (State, drawUI, handleEvent, theMap) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import States
import StateManagement
import qualified Graphics.Vty as V

drawUI :: IS -> [Widget Name]
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

handleEvent :: GlobalState -> IS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s (VtyEvent e) =
  let update = updateIS gs
      continue' = continue . update
      halt' = continue . popState in
    case e of
      V.EvKey V.KEsc [] -> halt' gs
      V.EvKey V.KEnter [] -> halt' gs
      V.EvKey V.KDown [] -> vScrollBy (viewportScroll Ordinary) 1 >> continue' s
      V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll Ordinary) 1 >> continue' s
      V.EvKey V.KUp [] -> vScrollBy (viewportScroll Ordinary) (-1) >> continue' s
      V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll Ordinary) (-1) >> continue' s
      _ -> continue' s
handleEvent gs _ _ = continue gs

titleAttr :: AttrName
titleAttr = attrName "title"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow) ]

drawInfo :: Widget Name
drawInfo = 
  padLeftRight 1 $
  vLimitPercent 60 $
  viewport Ordinary Vertical (strWrap info)

info :: String
info = unlines
  [ "Hascard is a text-based user interface for reviewing notes using 'flashcards'. Cards are written in markdown-like syntax; for more info see the README file. Use the --help flag for information on the command line options."
  , ""
  , "Controls:"
  , " * Use arrows or the j and k keys for menu navigation"
  , ""
  , " * Press the s key to toggle shuffling inside the deck selector menu"
  , ""
  , " * Enter confirms a selection, flips a card or continues to the next card"
  , ""
  , " * Use TAB or the arrow keys for navigating gaps in open questions"
  , ""
  , " * Use the c key for confirming reorder questions or multiple choice questions with more than 1 possible answer"
  , ""
  , " * Use F1 to show the answers of a open question"
  , ""
  , " * Use CTRL+Left and CTRL+Right to move to previous and next cards without having to answer them; this is disabled in review mode"]