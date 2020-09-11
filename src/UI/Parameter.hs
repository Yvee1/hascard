module UI.Parameter (drawUI, theMap, handleEvent) where

import UI.Attributes
import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Lens.Micro.Platform
import States
import StateManagement
import Runners
import qualified Graphics.Vty as V

drawUI :: PS -> [Widget Name]
drawUI = (:[]) . ui

ui :: PS -> Widget Name
ui f =
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hLimit 40 $
  hCenter (withAttr titleAttr (str "Select parameters")) <=>
  hBorder <=>
  padLeftRight 1
  (str "Filler")

handleEvent :: GlobalState -> PS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs form ev@(VtyEvent e) =
  let update = updatePS gs
      continue' = continue . update
      halt' = continue . popState
  in case e of
    -- continue gs
      V.EvKey V.KEsc []         -> halt' gs
      V.EvKey V.KEnter []       -> continue =<< (gs `moveToState`) <$> liftIO (cardsWithOptionsState gs (form ^. psFp) (form ^. psCards))
      _                         -> continue gs

handleEvent gs _ _ = continue gs