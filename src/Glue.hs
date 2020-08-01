module Glue where
import Brick
import States
import UI.Cards (Card)
import qualified Brick.Widgets.List as L
import qualified Stack
import qualified Data.Vector      as Vec
import qualified UI.MainMenu      as MM
import qualified UI.Settings      as S
import qualified UI.Info          as I
import qualified UI.CardSelector  as CS
import qualified UI.FileBrowser   as FB
import qualified UI.Cards         as C

globalApp :: App GlobalState () ()
globalApp = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = handleAttrMap
  }

drawUI :: GlobalState -> [Widget Name]
drawUI gs = case getState gs of
  MainMenuState     s -> MM.drawUI s
  SettingsState     s ->  S.drawUI s
  InfoState         s ->  I.drawUI s
  CardSelectorState s -> CS.drawUI s
  FileBrowserState  s -> FB.drawUI s
  CardsState        s ->  C.drawUI s

handleEvent :: GlobalState -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs ev = case getState gs of
  MainMenuState     s -> MM.handleEvent gs s ev
  SettingsState     s ->  S.handleEvent gs s ev
  InfoState         s ->  I.handleEvent gs s ev
  CardSelectorState s -> CS.handleEvent gs s ev
  FileBrowserState  s -> FB.handleEvent gs s ev
  CardsState        s ->  C.handleEvent gs s ev

handleAttrMap :: GlobalState -> AttrMap
handleAttrMap gs = case getState gs of
  MainMenuState     _ -> MM.theMap
  SettingsState     _ ->  S.theMap
  InfoState         _ ->  I.theMap
  CardSelectorState _ -> CS.theMap
  FileBrowserState  _ -> FB.theMap
  CardsState        _ ->  C.theMap

runCardsUI :: GlobalState -> [Card] -> IO GlobalState
runCardsUI gs deck = do
  hints    <- S.getShowHints
  controls <- S.getShowControls

  let initialState = 
        C.State { C._cards = deck
              , C._index = 0
              , C._currentCard = head deck
              , C._cardState = C.defaultCardState (head deck)
              , C._nCards = length deck
              , C._showHints = hints
              , C._showControls = controls }

  return $ goToState initialState

runMainMenuUI :: GlobalState -> GlobalState
runMainMenuUI gs = 
  let options = Vec.fromList 
                  [ "Select"
                  , "Info"
                  , "Settings"
                  , "Quit" ]

      initialState = MM.State (L.list () options 1) gs in 
        goToState initialState