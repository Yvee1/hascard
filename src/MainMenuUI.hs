module MainMenuUI (runMainMenuUI) where

import Brick
import BrickHelpers
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import CardSelectorUI
import Data.Functor (($>))
import InfoUI
import SettingsUI
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

type Event = ()
type Name = ()
type State = L.List Name String

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

title :: Widget Name
title = withAttr titleAttr $
        str "┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐┌┬┐" <=>
        str "├─┤├─┤└─┐│  ├─┤├┬┘ ││" <=>
        str "┴ ┴┴ ┴└─┘└─┘┴ ┴┴└──┴┘" 

drawUI :: State -> [Widget Name]
drawUI s = 
  [ drawMenu s ]

drawMenu :: State -> Widget Name
drawMenu s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  -- hLimit 21 $
  hLimitPercent 60 $
  hLimit 40 $
  hCenter title <=>
  hBorder <=>
  drawList s

drawList :: State -> Widget Name
drawList s = vLimit 4 $
             L.renderList drawListElement True s

drawListElement :: Bool -> String -> Widget Name
drawListElement selected = hCenteredStrWrapWithAttr attr
  where attr = if selected then withAttr selectedAttr else id

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.defAttr)
    , (selectedAttr,    fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow) ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent l (VtyEvent e) =
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt l
      V.EvKey V.KEsc [] -> halt l
      V.EvKey V.KEnter [] ->
        case L.listSelected l of
          Just 0 -> suspendAndResume $ runCardSelectorUI $> l
          Just 1 -> suspendAndResume $ runInfoUI $> l
          Just 2 -> suspendAndResume $ runSettingsUI $> l
          Just 3 -> halt l
          _ -> undefined

      ev -> continue =<< L.handleListEventVi L.handleListEvent ev l
handleEvent l _ = continue l

runMainMenuUI :: IO ()
runMainMenuUI = do
  let options = Vec.fromList [ "Select"
                             , "Info"
                             , "Settings"
                             , "Quit" ]

  let initialState = L.list () options 1
  _ <- defaultMain app initialState
  return ()

--   _    _                             _ 
--  | |  | |                           | |
--  | |__| | __ _ ___  ___ __ _ _ __ __| |
--  |  __  |/ _` / __|/ __/ _` | '__/ _` |
--  | |  | | (_| \__ \ (_| (_| | | | (_| |
--  |_|  |_|\__,_|___/\___\__,_|_|  \__,_|
                                       
--   _                                 _ 
--  | |                               | |
--  | |__   __ _ ___  ___ __ _ _ __ __| |
--  | '_ \ / _` / __|/ __/ _` | '__/ _` |
--  | | | | (_| \__ \ (_| (_| | | | (_| |
--  |_| |_|\__,_|___/\___\__,_|_|  \__,_|
                                      
-- ┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐┌┬┐
-- ├─┤├─┤└─┐│  ├─┤├┬┘ ││
-- ┴ ┴┴ ┴└─┘└─┘┴ ┴┴└──┴┘
