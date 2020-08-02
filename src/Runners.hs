module Runners where
import Brick.Widgets.FileBrowser
import DeckHandling
import Recents
import Settings
import States
import Types
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Stack as S

runCardSelectorUI :: GlobalState -> IO GlobalState
runCardSelectorUI gs = do
  rs <- getRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
  let options = Vec.fromList (prettyRecents ++ ["Select file from system"])
  let initialState = CSS (L.list () options 1) Nothing rs
  return $ gs `goToState` CardSelectorState initialState

runMainMenuUI :: GlobalState -> GlobalState
runMainMenuUI gs = 
  let options = Vec.fromList 
                  [ "Select"
                  , "Info"
                  , "Settings"
                  , "Quit" ]

      initialState = MMS (L.list () options 1) in
  gs `goToState` MainMenuState initialState

runCardsUI :: GlobalState -> [Card] -> IO GlobalState
runCardsUI gs deck = do
  hints    <- getShowHints
  controls <- getShowControls

  let initialState = 
        CS { _cards = deck
           , _index = 0
           , _currentCard = head deck
           , _cardState = defaultCardState (head deck)
           , _nCards = length deck
           , _showHints = hints
           , _showControls = controls }
 
  return $ gs `goToState` CardsState initialState

runCardsWithOptions :: GlobalState -> [Card] -> IO GlobalState
runCardsWithOptions state cards = doRandomization state cards >>= runCardsUI state

runSettingsUI :: GlobalState -> IO GlobalState
runSettingsUI gs = do
  currentSettings <- getSettings
  return $ gs `goToState` SettingsState (0, currentSettings)

runInfoUI :: GlobalState -> GlobalState
runInfoUI = (`goToState` InfoState ())

runFileBrowserUI :: GlobalState -> IO GlobalState
runFileBrowserUI gs = do
  browser <- newFileBrowser selectNonDirectories () Nothing
  let filteredBrowser = setFileBrowserEntryFilter (Just (entryFilter False)) browser
  return $ gs `goToState` FileBrowserState (FBS filteredBrowser Nothing [] Nothing False)

entryFilter :: Bool -> FileInfo -> Bool
entryFilter acceptHidden info = fileExtensionMatch "txt" info && (acceptHidden || 
  case fileInfoFilename info of
    ".."    -> True
    '.' : _ -> False
    _       -> True)