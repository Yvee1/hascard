module UI.MainMenu (State (..), drawUI, handleEvent, theMap) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Runners
import Settings
import States
import StateManagement
import UI.Attributes
import UI.BrickHelpers
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

title :: Widget Name
title = withAttr titleAttr $
        str "┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐┌┬┐" <=>
        str "├─┤├─┤└─┐│  ├─┤├┬┘ ││" <=>
        str "┴ ┴┴ ┴└─┘└─┘┴ ┴┴└──┴┘"

drawUI :: MMS -> [Widget Name]
drawUI s =
  [ drawMenu s ]

drawMenu :: MMS -> Widget Name
drawMenu s =
  joinBorders $
  center $
  withBorderStyle unicodeRounded $
  border $
  hLimit 40 $
  hCenter title <=>
  hBorder <=>
  drawList s

drawList :: MMS -> Widget Name
drawList s = vLimit 4 $
             L.renderList drawListElement True (s^.l)

drawListElement :: Bool -> String -> Widget Name
drawListElement selected = hCenteredStrWrapWithAttr attr
  where attr = if selected then withAttr selectedAttr else id

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> halt
  V.EvKey (V.KChar 'q') []  -> halt
  V.EvKey V.KEnter [] -> do
    list <- use (mms.l)
    case L.listSelected list of
      Just 0 -> goToState =<< liftIO cardSelectorState
      Just 1 -> goToState infoState
      Just 2 -> goToState =<< liftIO settingsState
      Just 3 -> halt
      _ -> undefined

  ev -> zoom (mms.l) $ L.handleListEventVi L.handleListEvent ev
handleEvent _ = return ()
