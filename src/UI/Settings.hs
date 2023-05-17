module UI.Settings (State, drawUI, handleEvent, theMap) where

import UI.Attributes
import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (unless)
import Control.Monad.IO.Class
import Lens.Micro.Platform
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

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent ev@(VtyEvent e) = do
  form <- use ss
  let halt' = popState <* liftIO (setSettings (formState form))
      focus = formFocus form
      (Just n) = focusGetCurrent focus
      down = unless (n == MaxRecentsField) $
               ss .= form { formFocus = focusNext focus }
      up = unless (n == HintsField) $
             ss .= form { formFocus = focusPrev focus }

      
  case e of
    V.EvKey V.KEsc []         -> halt'
    V.EvKey (V.KChar 'q') []  -> halt'
    V.EvKey V.KDown []        -> down
    V.EvKey (V.KChar 'j') []  -> down
    V.EvKey V.KUp []          -> up
    V.EvKey (V.KChar 'k') []  -> up
    V.EvKey (V.KChar '\t') [] -> return ()
    V.EvKey V.KBackTab []     -> return ()
    _ -> zoom ss $ handleFormEvent ev

handleEvent _ = return ()
