{-# LANGUAGE TemplateHaskell #-}
module States where

import Brick.Widgets.FileBrowser
import Brick.Widgets.List (List)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Lens.Micro.Platform
import System.Random.MWC (GenIO)
import Stack hiding (head)
import Types
import qualified Data.Map.Strict as Map
import qualified Stack

type Name = ()
type Event = ()

data Mode  = MainMenu    
           | Settings    
           | Info        
           | CardSelector
           | FileBrowser 
           | Cards
  deriving (Show, Eq, Ord)

data State = MainMenuState     MMS
           | SettingsState     SS
           | InfoState         IS
           | CardSelectorState CSS
           | FileBrowserState  FBS
           | CardsState        CS

data GlobalState = GlobalState
  { _mwc        :: GenIO
  , _doShuffle  :: Bool
  , _subset     :: Maybe Int
  , _stack      :: Stack Mode
  , _states     :: Map Mode State
  }

data CardState = 
    DefinitionState
  { _flipped        :: Bool }
  | MultipleChoiceState
  { _highlighted    :: Int
  , _number         :: Int
  , _tried          :: Map Int Bool      -- indices of tried choices
  }
  | MultipleAnswerState
  { _highlighted    :: Int
  , _selected       :: Map Int Bool
  , _number         :: Int
  , _entered        :: Bool
  }
  | OpenQuestionState
  { _gapInput       :: Map Int String
  , _highlighted    :: Int
  , _number         :: Int
  , _entered        :: Bool
  , _correctGaps    :: Map Int Bool
  }
  | ReorderState
  { _highlighted    :: Int
  , _grabbed        :: Bool 
  , _order          :: Map Int (Int, String)
  , _entered        :: Bool
  , _number         :: Int
  }

data CS = CS
  { _cards          :: [Card]     -- list of flashcards
  , _index          :: Int        -- current card index
  , _nCards         :: Int        -- number of cards
  , _currentCard    :: Card
  , _cardState      :: CardState
  , _showHints      :: Bool
  , _showControls   :: Bool
  -- , _incorrectCards :: [Int]      -- list of indices of incorrect answers
  }

newtype MMS = MMS 
  { _l  :: List Name String }

type IS = ()

type Settings = Map Int Bool
type SS = (Int, Settings)

data CSS = CSS
  { _list       :: List Name String
  , _exception  :: Maybe String
  , _recents    :: Stack FilePath
  }

data FBS = FBS
  { _fb          :: FileBrowser Name
  , _exception'  :: Maybe String
  , _parsedCards :: [Card]
  , _filePath    :: Maybe FilePath
  , _showHidden  :: Bool
  }

makeLenses ''State
makeLenses ''MMS
makeLenses ''GlobalState
makeLenses ''CardState
makeLenses ''CS
makeLenses ''CSS
makeLenses ''FBS

getState :: GlobalState -> State
getState = fromJust . safeGetState

updateState :: GlobalState -> State -> GlobalState
updateState gs s@(MainMenuState _)     = gs & states %~ Map.insert MainMenu s
updateState gs s@(SettingsState _)     = gs & states %~ Map.insert Settings s
updateState gs s@(InfoState _)         = gs & states %~ Map.insert Info s
updateState gs s@(CardSelectorState _) = gs & states %~ Map.insert CardSelector s
updateState gs s@(FileBrowserState _)  = gs & states %~ Map.insert FileBrowser s
-- updateState gs s@(MainMenuState _) = gs & states %~ Map.insert MainMenu s

updateMMS :: GlobalState -> MMS -> GlobalState
updateMMS gs s = updateState gs (MainMenuState s)

updateSS :: GlobalState -> SS -> GlobalState
updateSS gs s = updateState gs (SettingsState s)

updateCS :: GlobalState -> CS -> GlobalState
updateCS gs s = updateState gs (CardsState s)

updateCSS :: GlobalState -> CSS -> GlobalState
updateCSS gs s = updateState gs (CardSelectorState s)

updateInfo :: GlobalState -> IS -> GlobalState
updateInfo gs s = updateState gs (InfoState s)

updateFBS :: GlobalState -> FBS -> GlobalState
updateFBS gs s = updateState gs (FileBrowserState s)

goToState :: State -> GlobalState -> GlobalState
goToState s@(MainMenuState _) gs     = gs & states %~ Map.insert MainMenu s
                                          & stack  %~ insert MainMenu
goToState s@(SettingsState _) gs     = gs & states %~ Map.insert Settings s
                                          & stack  %~ insert Settings
goToState s@(InfoState _) gs         = gs & states %~ Map.insert Info s
                                          & stack  %~ insert Info
goToState s@(CardSelectorState _) gs = gs & states %~ Map.insert CardSelector s
                                          & stack  %~ insert CardSelector
goToState s@(FileBrowserState _) gs  = gs & states %~ Map.insert FileBrowser s
                                          & stack  %~ insert FileBrowser

safeGetState :: GlobalState -> Maybe State
safeGetState gs = do
  key <- safeHead (gs ^. stack)
  Map.lookup key (gs ^. states)