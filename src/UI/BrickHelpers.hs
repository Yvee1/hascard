module UI.BrickHelpers where
import Text.Wrap
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Text (pack)
import Graphics.Vty (imageWidth, imageHeight, charFill)
import Lens.Micro
import UI.Attributes

hCenteredStrWrap :: String -> Widget n
hCenteredStrWrap = hCenteredStrWrapWithAttr id

hCenteredStrWrapWithAttr :: (Widget n -> Widget n) -> String -> Widget n
hCenteredStrWrapWithAttr attr p = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  let result = vBox $ map (hCenter . attr . txt) $ wrapTextToLines (WrapSettings {preserveIndentation=False, breakLongWords=True}) w (pack p)
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
hFill ch =
    Widget Greedy Fixed $ do
      c <- getContext
      return $ emptyResult & imageL .~ charFill (c^.attrL) ch (c^.availWidthL) 1