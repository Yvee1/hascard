module MainMenuUI (runMainMenuUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import CardUI
import CardSelectorUI
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

type Event = ()
type Name = ()
type State = L.List Name String

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

title :: Widget Name
title = withAttr titleAttr $
        str "┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐┌┬┐" <=>
        str "├─┤├─┤└─┐│  ├─┤├┬┘ ││" <=>
        str "┴ ┴┴ ┴└─┘└─┘┴ ┴┴└──┴┘" 
        -- str " _                                 _" <=>
        -- str "| |                               | |"<=>
        -- str "| |__   __ _ ___  ___ __ _ _ __ __| |"<=>
        -- str "| '_ \\ / _` / __|/ __/ _` | '__/ _` |"<=>
        -- str "| | | | (_| \\__ \\ (_| (_| | | | (_| |"<=>
        -- str "|_| |_|\\__,_|___/\\___\\__,_|_|  \\__,_|"


drawUI :: State -> [Widget Name]
drawUI s = 
  [ drawMenu s ]

drawMenu :: State -> Widget Name
drawMenu s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  -- hLimit 21 $
  hLimitPercent 60 $
  hCenter title <=>
  hBorder <=>
  hCenter (drawList s)

drawList :: State -> Widget Name
drawList s = hLimit 11 $
             vLimit 3  $
             L.renderList drawListElement True s

drawListElement :: Bool -> String -> Widget Name
drawListElement selected text = 
  hCenter $
  attr $
  str text
  where attr = if selected then withAttr selectedAttr else id

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.defAttr)
    , (selectedAttr,    fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow) ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent l (VtyEvent e) =
    case e of
      V.EvKey (V.KChar 'q') [] -> halt l
      V.EvKey V.KEsc [] -> halt l
      V.EvKey V.KEnter [] -> do
        case L.listSelected l of
          Just 0 -> suspendAndResume $ do runCardSelectorUI
                                          return l
          Just 1 -> undefined
          Just 2 -> halt l
          _ -> undefined

      ev -> continue =<< L.handleListEventVi L.handleListEvent ev l
handleEvent l _ = continue l

runMainMenuUI :: IO ()
runMainMenuUI = do
  let options = Vec.fromList $ [ "Start"
                               , "Info"
                               , "Quit" ]

  let initialState = L.list () options 1
  _ <- defaultMain app initialState
  return ()

--   _    _                             _ 
--  | |  | |                           | |
--  | |__| | __ _ ___  ___ __ _ _ __ __| |
--  |  __  |/ _` / __|/ __/ _` | '__/ _` |
--  | |  | | (_| \__ \ (_| (_| | | | (_| |
--  |_|  |_|\__,_|___/\___\__,_|_|  \__,_|
                                       
--   _                                 _ 
--  | |                               | |
--  | |__   __ _ ___  ___ __ _ _ __ __| |
--  | '_ \ / _` / __|/ __/ _` | '__/ _` |
--  | | | | (_| \__ \ (_| (_| | | | (_| |
--  |_| |_|\__,_|___/\___\__,_|_|  \__,_|
                                      
-- ┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐┌┬┐
-- ├─┤├─┤└─┐│  ├─┤├┬┘ ││
-- ┴ ┴┴ ┴└─┘└─┘┴ ┴┴└──┴┘
