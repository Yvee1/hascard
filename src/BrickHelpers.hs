module BrickHelpers where
import Text.Wrap
import Brick
import Brick.Widgets.Center
import Data.Text (pack)
import Lens.Micro

-- hCenteredStrWrap :: String -> Widget ()
-- hCenteredStrWrap = myStrWrap' 

hCenteredStrWrap :: String -> Widget n
hCenteredStrWrap p = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  let result = vBox $ map (hCenter . txt) $ wrapTextToLines defaultWrapSettings w (pack p)
  render result