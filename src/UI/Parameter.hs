module UI.Parameter (drawUI, theMap, handleEvent) where

import UI.Attributes
import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (when)
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

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent ev@(VtyEvent e) = do
  s <- use ps
  let form = s ^. psForm
      focus = formFocus form
      (Just n) = focusGetCurrent focus
      down = zoom ps $ case n of
        ParametersOkField -> return ()
        ChunkField1 -> psForm .= form { formFocus = focusNext (focusNext focus) }
        _ -> psForm .= form { formFocus = focusNext focus }
      up = zoom ps $ case n of
        ChunkField1 -> return ()
        ChunkField2 -> return ()
        SubsetField -> psForm .= form { formFocus = focusPrev (focusPrev focus) }
        _           -> psForm .= form { formFocus = focusPrev focus }
      right = zoom ps $ case n of
        ChunkField1 -> psForm .= form { formFocus = focusNext focus }
        _ -> return ()
      left = zoom ps $ case n of
        ChunkField2 -> psForm .= form { formFocus = focusPrev focus }
        _ -> return ()

  case e of
      V.EvKey V.KEsc []         -> popState
      V.EvKey (V.KChar 'q') []  -> popState
      V.EvKey V.KDown []        -> down
      V.EvKey (V.KChar 'j') []  -> down
      V.EvKey V.KUp []          -> up
      V.EvKey (V.KChar 'k') []  -> up
      V.EvKey (V.KChar '\t') [] -> return ()
      V.EvKey V.KBackTab []     -> return ()
      V.EvKey (V.KChar 'h') []  -> left
      V.EvKey (V.KChar 'l') []  -> right
    
      _ -> case (e, n) of
          (V.EvKey V.KRight [], ChunkField2) -> return ()
          (V.EvKey V.KLeft [],  ChunkField1) -> return ()
          (V.EvKey V.KRight [], SubsetField) -> return ()
          (V.EvKey V.KLeft [],  SubsetField) -> return ()
          _ -> do zoom (ps.psForm) $ handleFormEvent ev
                  f <- use $ ps.psForm
                  when (formState f ^. pOk) $ do
                    parameters .= formState f
                    parameters.pOk .= False
                    paramsWithoutOk <- use parameters
                    ps.psForm .= updateFormState paramsWithoutOk f
                    state <- cardsWithOptionsStateM (s ^. psFp) (s ^. psCards)
                    goToState state

handleEvent _ = return ()
