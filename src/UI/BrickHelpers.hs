{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module UI.BrickHelpers where
import Text.Wrap
import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Char (isDigit)
import Data.Maybe
import Data.Text (pack)
import Graphics.Vty (imageWidth, imageHeight, charFill)
import Lens.Micro
import Text.Read (readMaybe)
import UI.Attributes
import qualified Graphics.Vty as V

hCenteredStrWrap :: String -> Widget n
hCenteredStrWrap = hCenteredStrWrapWithAttr id

hCenteredStrWrapWithAttr :: (Widget n -> Widget n) -> String -> Widget n
hCenteredStrWrapWithAttr attr p = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  let result = vBox $ map (hCenter . attr . txt) $ wrapTextToLines (defaultWrapSettings {preserveIndentation=False, breakLongWords=True}) w (pack p)
  render result

-- Somewhat inefficient because rendering is done just to
-- determine the width and height. So don't use this if the
-- rendering is expensive.
centerPopup :: Widget n -> Widget n
centerPopup widget = Widget Fixed Fixed $ do
  c <- getContext
  result <- render widget
  let w = result^.imageL.to imageWidth
      h = result^.imageL.to imageHeight
      x = (c^.availWidthL - w) `div` 2
      y = (c^.availHeightL - h) `div` 2
  render $ translateBy (Location (x, y)) widget

drawException :: Maybe String -> Widget n
drawException Nothing = emptyWidget
drawException (Just e) =
        centerPopup $ 
        borderWithLabel (str "Error") $
        withAttr exceptionAttr $ str e

-- | Fill all available space with the specified character. Grows only
-- horizontally.
hFill :: Char -> Widget n
hFill = vLimit 1 . fill

-- | Fill all available space with the specified character. Grows only
-- vertically.
vFill :: Char -> Widget n
vFill = hLimit 1 . fill

atLeastV :: Int -> Widget n -> Widget n
atLeastV n widget = Widget Fixed Fixed $ do
  c <- getContext
  result <- render widget
  let h  = result^.imageL.to imageHeight
      dh = n - h
  if dh > 0 then render $ vLimit n (widget <=> vFill ' ') else render widget

yesnoField :: (Ord n, Show n) => Bool -> Lens' s Bool -> n -> String -> s -> FormFieldState s e n
yesnoField rightAlign stLens name label initialState =
  let initVal = initialState ^. stLens

      handleEvent (MouseDown n _ _ _) | n == name = modify not
      handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify not
      handleEvent (VtyEvent (V.EvKey V.KEnter [])) = modify not
      handleEvent _ = return ()
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderYesno rightAlign label name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldUpdate = const
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderYesno :: Ord n => Bool -> String -> n -> Bool -> Bool -> Widget n
renderYesno rightAlign label n foc val =
  let addAttr = if foc then withDefAttr focusedFormInputAttr else id
  in clickable n $
    (if val 
      then addAttr (str "Yes")
      else if rightAlign 
        then str " " <+> addAttr (str "No")
        else addAttr (str "No") <+> str " ") <+> str label

naturalNumberField :: (Ord n, Show n) => Int -> Lens' s Int -> n -> String -> s -> FormFieldState s e n
naturalNumberField bound stLens name postfix initialState =
  let initVal = initialState ^. stLens

      handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c =
           do s <- get
              let newValue = read (show s ++ [c])
              put $ min newValue bound
      handleEvent (VtyEvent (V.EvKey V.KBS [])) = 
        do s <- get
           put $ case show s of
             "" -> 0
             xs -> fromMaybe 0 (readMaybe (init xs))
      handleEvent _ = return ()
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderNaturalNumber bound postfix name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldUpdate = const
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderNaturalNumber :: Int -> String -> n -> Bool -> Int -> Widget n
renderNaturalNumber bound postfix n foc val =
  let addAttr = if foc then withDefAttr focusedFormInputAttr else id
      val' = show val
      csr = if foc then showCursor n (Location (length val',0)) else id
  in if null postfix
    then hLimit (length (show bound)) (csr (addAttr (str val')) <+> hFill ' ') 
    else csr (addAttr (str val')) <+> str postfix
