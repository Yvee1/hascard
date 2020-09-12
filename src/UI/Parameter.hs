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
ui s =
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hLimit 40 $
  hCenter (withAttr titleAttr (str "Select parameters")) <=>
  hBorder <=>
  padLeftRight 1 
  (renderForm (s ^. psForm))

handleEvent :: GlobalState -> PS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s ev@(VtyEvent e) =
  let form = s ^. psForm

      update = updatePS gs
      continue' = continue . update
      continue'' f = continue . update $ s & psForm .~ f

      halt' = continue . popState

      focus = formFocus form
      (Just n) = focusGetCurrent focus
      down = if n == ParametersOkField then continue gs
        else continue'' $ form { formFocus = focusNext focus }
      up = if n == ShuffleField then continue gs
        else continue'' $ form { formFocus = focusPrev focus }

  in case e of
    -- continue gs
      V.EvKey V.KEsc []         -> halt' gs
      V.EvKey V.KDown []        -> down
      V.EvKey (V.KChar 'j') []  -> down
      V.EvKey V.KUp []          -> up
      V.EvKey (V.KChar 'k') []  -> up
      V.EvKey (V.KChar '\t') [] -> continue gs
      V.EvKey V.KBackTab []     -> continue gs
      _                         -> do f <- handleFormEvent ev form
                                      if formState f ^. pOk
                                        then continue =<< (gs `moveToState`) 
                                             <$> liftIO (cardsWithOptionsState 
                                                         (gs & parameters .~ formState f)
                                                         (s ^. psFp)
                                                         (s ^. psCards))
                                        else continue' (s & psForm .~ f)

handleEvent gs _ _ = continue gs
