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

cardSelectorState :: IO State
cardSelectorState = do
  rs <- getRecents
  maxRs <- getMaxRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
      options = Vec.fromList (prettyRecents ++ ["Select file from system"])
      initialState = CSS
        { _list = L.list Ordinary options 1
        , _exception = Nothing
        , _recents = rs
        , _maxRecentsToShow = maxRs }
  return $ CardSelectorState initialState

mainMenuState :: State
mainMenuState = 
  let options = Vec.fromList 
                  [ "Select"
                  , "Info"
                  , "Settings"
                  , "Quit" ]

      initialState = MMS (L.list Ordinary options 1) in
  MainMenuState initialState

cardsState :: [Card] -> IO State
cardsState deck = do
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
 
  return $ CardsState initialState

cardsWithOptionsState :: GlobalState -> [Card] -> IO State
cardsWithOptionsState gs cards = doRandomization gs cards >>= cardsState

infoState :: State
infoState = InfoState ()

fileBrowserState :: IO State
fileBrowserState = do
  browser <- newFileBrowser selectNonDirectories Ordinary Nothing
  let filteredBrowser = setFileBrowserEntryFilter (Just (entryFilter False)) browser
  return $ FileBrowserState (FBS filteredBrowser Nothing [] Nothing False)

entryFilter :: Bool -> FileInfo -> Bool
entryFilter acceptHidden info = fileExtensionMatch "txt" info && (acceptHidden || 
  case fileInfoFilename info of
    ".."    -> True
    '.' : _ -> False
    _       -> True)
