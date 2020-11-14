module Runners where
import Brick.Widgets.FileBrowser
import Brick.Forms
import DeckHandling
import Data.Maybe (fromMaybe)
import Recents
import Lens.Micro.Platform
import Parameters
import Settings
import States
import System.FilePath (takeDirectory)
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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

cardsState :: Bool -> FilePath -> [Card] -> IO State
cardsState doReview fp deck = do
  hints    <- getShowHints
  controls <- getShowControls

  let mFirstCard = safeHead deck
      firstCard = fromMaybe (Definition "Empty deck" Nothing "Click enter to go back.") mFirstCard
      deck' = maybe [firstCard] (const deck) mFirstCard

      initialState = 
        CS { _cards = deck'
           , _index = 0
           , _currentCard = firstCard
           , _cardState = defaultCardState firstCard
           , _nCards = length deck'
           , _showHints = hints
           , _showControls = controls
           , _reviewMode = maybe False (const doReview) mFirstCard
           , _correctCards = []
           , _popup = Nothing
           , _pathToFile = fp }
 
  openCardImage (takeDirectory fp) firstCard
  return $ CardsState initialState

cardsWithOptionsState :: GlobalState -> FilePath -> [Card] -> IO State
cardsWithOptionsState gs fp cards =
  let chunked = doChunking (gs^.parameters.pChunk) cards
  in doRandomization gs chunked >>= cardsState (gs^.parameters.pReviewMode) fp

infoState :: State
infoState = InfoState ()

fileBrowserState :: IO State
fileBrowserState = do
  browser <- newFileBrowser selectNonDirectories Ordinary Nothing
  let filteredBrowser = setFileBrowserEntryFilter (Just (entryFilter False)) browser
  return $ FileBrowserState (FBS filteredBrowser Nothing [] Nothing False)

entryFilter :: Bool -> FileInfo -> Bool
entryFilter acceptHidden info = (fileExtensionMatch "txt" info || fileExtensionMatch "md" info) && (acceptHidden || 
  case fileInfoFilename info of
    ".."    -> True
    '.' : _ -> False
    _       -> True)

parameterState :: Parameters -> FilePath -> [Card] -> State
parameterState ps fp cards = ParameterState (PS cards fp (mkParameterForm (length cards) ps))