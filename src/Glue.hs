module Glue where
import Brick
import Control.Monad (when)
import Control.Monad.State.Lazy
import States
import StateManagement
import qualified Graphics.Vty     as V
import qualified UI.MainMenu      as MM
import qualified UI.Settings      as S
import qualified UI.Info          as I
import qualified UI.CardSelector  as CS
import qualified UI.FileBrowser   as FB
import qualified UI.Cards         as C
import qualified UI.Parameter     as P

globalApp :: App GlobalState Event Name
globalApp = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = enableMouse
  , appAttrMap = handleAttrMap
  }

enableMouse = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
      liftIO $ V.setMode output V.Mouse True

drawUI :: GlobalState -> [Widget Name]
drawUI gs = case evalState getState gs of
  MainMenuState     s -> MM.drawUI s
  SettingsState     s ->  S.drawUI s
  InfoState         s ->  I.drawUI s
  CardSelectorState s -> CS.drawUI gs s
  FileBrowserState  s -> FB.drawUI s
  CardsState        s ->  C.drawUI s
  ParameterState    s ->  P.drawUI s

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent ev = do
  if ev == VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) then halt else do
    s <- getState
    case s of
      MainMenuState     s -> MM.handleEvent ev
      SettingsState     s ->  S.handleEvent ev
      InfoState         s ->  I.handleEvent ev
      CardSelectorState s -> CS.handleEvent ev
      FileBrowserState  s -> FB.handleEvent ev
      CardsState        s ->  C.handleEvent ev
      ParameterState    s ->  P.handleEvent ev

handleAttrMap :: GlobalState -> AttrMap
handleAttrMap gs = case evalState getState gs of
  MainMenuState     _ -> MM.theMap
  SettingsState     _ ->  S.theMap
  InfoState         _ ->  I.theMap
  CardSelectorState _ -> CS.theMap
  FileBrowserState  _ -> FB.theMap
  CardsState        _ ->  C.theMap
  ParameterState    _ ->  P.theMap