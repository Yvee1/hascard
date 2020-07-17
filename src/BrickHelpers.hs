module BrickHelpers where
import Text.Wrap
import Brick
import Brick.Widgets.Center
import Data.Text (pack)
import Lens.Micro

hCenteredStrWrap :: String -> Widget n
hCenteredStrWrap = hCenteredStrWrapWithAttr id

hCenteredStrWrapWithAttr :: (Widget n -> Widget n) -> String -> Widget n
hCenteredStrWrapWithAttr attr p = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  let result = vBox $ map (hCenter . attr . txt) $ wrapTextToLines (WrapSettings {preserveIndentation=False, breakLongWords=True}) w (pack p)
  render result