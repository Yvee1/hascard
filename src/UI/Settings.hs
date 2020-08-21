module UI.Settings (State, drawUI, handleEvent, theMap) where

import UI.Attributes
import Brick hiding (mergeWithDefault)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import States
import StateManagement
import Settings
import qualified Graphics.Vty as V

drawUI :: SS -> [Widget Name]
drawUI = (:[]) . ui

ui :: SS -> Widget Name
ui f =
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hLimit 40 $
  hCenter (withAttr titleAttr (str "Settings")) <=>
  hBorder <=>
  padLeftRight 1
  (renderForm f)

handleEvent :: GlobalState -> SS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs form ev@(VtyEvent e) =
  let update = updateSS gs
      continue' = continue . update
      halt' global = continue (popState global) <* liftIO (setSettings (formState form))
      
      focus = formFocus form
      (Just n) = focusGetCurrent focus
      down = if n == MaxRecentsField then continue gs
        else continue' $ form { formFocus = focusNext focus }
      up = if n == HintsField then continue gs
        else continue' $ form { formFocus = focusPrev focus }

      in
    case e of
      V.EvKey V.KEsc []         -> halt' gs
      V.EvKey V.KDown []        -> down
      V.EvKey (V.KChar 'j') []  -> down
      V.EvKey V.KUp []          -> up
      V.EvKey (V.KChar 'k') []  -> up
      V.EvKey (V.KChar '\t') [] -> continue gs
      V.EvKey V.KBackTab []     -> continue gs
      _ -> continue' =<< handleFormEvent ev form

handleEvent gs _ _ = continue gs
