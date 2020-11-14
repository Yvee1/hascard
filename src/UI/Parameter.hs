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
      down = case n of
        ParametersOkField -> continue gs
        ChunkField1 -> continue'' $ form { formFocus = focusNext (focusNext focus) }
        _ -> continue'' $ form { formFocus = focusNext focus }
      up = case n of
        ChunkField1 -> continue gs
        ChunkField2 -> continue gs
        SubsetField -> continue'' $ form { formFocus = focusPrev (focusPrev focus) }
        _           -> continue'' $ form { formFocus = focusPrev focus }

  in case e of
      V.EvKey V.KEsc []         -> halt' gs
      V.EvKey (V.KChar 'q') []  -> halt' gs
      V.EvKey V.KDown []        -> down
      V.EvKey (V.KChar 'j') []  -> down
      V.EvKey V.KUp []          -> up
      V.EvKey (V.KChar 'k') []  -> up
      V.EvKey (V.KChar '\t') [] -> continue gs
      V.EvKey V.KBackTab []     -> continue gs
    
      _ -> case (e, n) of
          (V.EvKey V.KRight [], ChunkField2) -> continue gs
          (V.EvKey V.KLeft [],  ChunkField1) -> continue gs
          _ -> do f <- handleFormEvent ev form
                  if formState f ^. pOk
                    then continue =<< (gs `goToState`)
                          <$> liftIO (cardsWithOptionsState
                                      (gs & parameters .~ formState f)
                                      (s ^. psFp)
                                      (s ^. psCards))
                    else continue' (s & psForm .~ f)

handleEvent gs _ _ = continue gs
