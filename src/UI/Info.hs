module UI.Info (State, drawUI, handleEvent, theMap) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import States
import StateManagement
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import UI.BrickHelpers

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

scroll :: Int -> EventM Name s ()
scroll = vScrollBy (viewportScroll InfoViewport)

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent (VtyEvent e) = do
  case e of
    V.EvKey V.KEsc [] -> popState
    V.EvKey (V.KChar 'q') [] -> popState
    V.EvKey V.KEnter [] -> popState
    V.EvKey V.KDown [] -> scroll 1
    V.EvKey (V.KChar 'j') [] -> scroll 1
    V.EvKey V.KUp [] -> scroll (-1)
    V.EvKey (V.KChar 'k') [] -> scroll (-1)
    _ -> return ()
handleEvent (T.MouseDown (SBClick el InfoViewport) _ _ _) = handleClickScroll scroll el
handleEvent _ = return ()

titleAttr :: AttrName
titleAttr = attrName "title"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow) ]

drawInfo :: Widget Name
drawInfo =
  padLeft (Pad 1) $
  scrollableViewportPercent 60 InfoViewport $
  strWrap info

info :: String
info = unlines
  [ "Hascard is a text-based user interface for reviewing notes using 'flashcards'. Cards are written in markdown-like syntax; for more info see the README file. Use the --help flag for information on the command line options."
  , ""
  , "Controls:"
  , " * Use arrows or the j and k keys for menu navigation"
  , ""
  , " * Enter confirms a selection, flips a card or continues to the next card"
  , ""
  , " * Use Tab or the arrow keys for navigating gaps in open questions"
  , ""
  , " * Use the c key for confirming reorder questions or multiple choice questions with more than 1 possible answer"
  , ""
  , " * Use Shift+Tab to show the answers of a open question"
  , ""
  , " * Use Ctrl+Left and Ctrl+Right to move to previous and next cards without having to answer them; this is disabled in review mode"]
