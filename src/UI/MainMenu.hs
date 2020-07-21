{-# LANGUAGE TemplateHaskell #-}
module UI.MainMenu (runMainMenuUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Functor (($>))
import Lens.Micro.Platform
import Types (GlobalState)
import UI.BrickHelpers
import UI.CardSelector
import UI.Info
import UI.Settings
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

type Event = ()
type Name = ()
data State = State 
  { _l  :: L.List Name String
  , _gs :: GlobalState }

makeLenses ''State

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
  hLimit 40 $
  hCenter title <=>
  hBorder <=>
  drawList s

drawList :: State -> Widget Name
drawList s = vLimit 4 $
             L.renderList drawListElement True (s^.l)

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
handleEvent s (VtyEvent e) =
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt s
      V.EvKey V.KEsc [] -> halt s
      V.EvKey V.KEnter [] ->
        case L.listSelected (s^.l) of
          Just 0 -> suspendAndResume $ runCardSelectorUI (s^.gs)$> s
          Just 1 -> suspendAndResume $ runInfoUI  $> s
          Just 2 -> suspendAndResume $ runSettingsUI $> s
          Just 3 -> halt s
          _ -> undefined

      ev -> continue . flip (l .~) s =<< L.handleListEventVi L.handleListEvent ev (s^.l)
handleEvent l _ = continue l

runMainMenuUI :: GlobalState -> IO ()
runMainMenuUI gs = do
  let options = Vec.fromList [ "Select"
                             , "Info"
                             , "Settings"
                             , "Quit" ]

  let initialState = State (L.list () options 1) gs
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
