{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module MainMenuUI where

import Brick
import System.Directory (getDirectoryContents)
import Parser
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

newtype State = State {
  files      :: [FilePath]
}
type Event = ()
type Name = ()

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr []

drawUI :: State -> [Widget Name]
drawUI s = [drawMenu s]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') []))         = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))                = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  = halt s
handleEvent s _ = halt s

drawMenu :: State -> Widget Name
drawMenu State{..} = 
  C.center $
  withBorderStyle BS.unicodeBold $
  B.border $
  str "Main menu"
  <=>
  vBox (map str files)

runMainMenuUI :: IO State
runMainMenuUI = do
  filePaths <- getDirectoryContents "cards"
  let initialState = State (filter (not . (`elem` [".", ".."])) filePaths)
  defaultMain app initialState