module UI.MainMenu (State (..), drawUI, handleEvent, theMap) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Runners
import States
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

handleEvent :: GlobalState -> MMS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s (VtyEvent e) =
  let update = updateMMS gs in
    case e of
      V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt gs
      V.EvKey V.KEsc [] -> halt gs
      V.EvKey V.KEnter [] ->
        case L.listSelected (s^.l) of
          Just 0 -> continue =<< (gs `goToState`) <$> liftIO cardSelectorState
          Just 1 -> continue $ gs `goToState` infoState
          Just 2 -> continue =<< (gs `goToState`) <$> liftIO settingsState 
          Just 3 -> halt gs
          _ -> undefined

      ev -> continue . update . flip (l .~) s =<< L.handleListEventVi L.handleListEvent ev (s^.l)
handleEvent gs _ _ = continue gs
