{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
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

-- class HasMode t where
--   getMode :: t -> Mode

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
-- instance HasMode CS where
--   getMode = const Cards

newtype MMS = MMS 
  { _l  :: List Name String }
-- instance HasMode MMS where
--   getMode = const MainMenu

type IS = ()
-- instance HasMode IS where
--   getMode = const Info

type Settings = Map Int Bool

type SS = (Int, Settings)
-- instance HasMode SS where
--   getMode = const Settings

data CSS = CSS
  { _list       :: List Name String
  , _exception  :: Maybe String
  , _recents    :: Stack FilePath
  }
-- instance HasMode CSS where
--   getMode = const CardSelector

data FBS = FBS
  { _fb          :: FileBrowser Name
  , _exception'  :: Maybe String
  , _parsedCards :: [Card]
  , _filePath    :: Maybe FilePath
  , _showHidden  :: Bool
  }
-- instance HasMode FBS where
--   getMode = const FileBrowser

getMode :: State -> Mode
getMode (MainMenuState     _) = MainMenu
getMode (SettingsState     _) = Settings
getMode (InfoState         _) = Info
getMode (CardSelectorState _) = CardSelector
getMode (FileBrowserState  _) = FileBrowser
getMode (CardsState        _) = Cards

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
-- updateState gs s@(MainMenuState _)     = gs & states %~ Map.insert MainMenu s
-- updateState gs s@(SettingsState _)     = gs & states %~ Map.insert Settings s
-- updateState gs s@(InfoState _)         = gs & states %~ Map.insert Info s
-- updateState gs s@(CardSelectorState _) = gs & states %~ Map.insert CardSelector s
-- updateState gs s@(FileBrowserState _)  = gs & states %~ Map.insert FileBrowser s
-- updateState gs s@(CardsState _)        = gs & states %~ Map.insert Cards s
updateState gs s = gs & states %~ Map.insert (getMode s) s

updateMMS :: GlobalState -> MMS -> GlobalState
updateMMS gs s = updateState gs (MainMenuState s)

updateSS :: GlobalState -> SS -> GlobalState
updateSS gs s = updateState gs (SettingsState s)

updateIS :: GlobalState -> IS -> GlobalState
updateIS gs s = updateState gs (InfoState s)

updateCS :: GlobalState -> CS -> GlobalState
updateCS gs s = updateState gs (CardsState s)

updateCSS :: GlobalState -> CSS -> GlobalState
updateCSS gs s = updateState gs (CardSelectorState s)

updateInfo :: GlobalState -> IS -> GlobalState
updateInfo gs s = updateState gs (InfoState s)

updateFBS :: GlobalState -> FBS -> GlobalState
updateFBS gs s = updateState gs (FileBrowserState s)

goToState :: GlobalState -> State -> GlobalState
-- goToState gs s@(MainMenuState _)     = gs & states %~ Map.insert MainMenu s
--                                           & stack  %~ insert MainMenu
-- goToState gs s@(SettingsState _)     = gs & states %~ Map.insert Settings s
--                                           & stack  %~ insert Settings
-- goToState gs s@(InfoState _)         = gs & states %~ Map.insert Info s
--                                           & stack  %~ insert Info
-- goToState gs s@(CardSelectorState _) = gs & states %~ Map.insert CardSelector s
--                                           & stack  %~ insert CardSelector
-- goToState gs s@(FileBrowserState _)  = gs & states %~ Map.insert FileBrowser s
--                                           & stack  %~ insert FileBrowser
-- goToState gs s@(CardsState _)        = gs & states %~ Map.insert Cards s
--                                           & stack  %~ insert Cards
goToState gs s = gs & states %~ Map.insert (getMode s) s
                    & stack  %~ insert (getMode s)

popState :: GlobalState -> GlobalState
popState gs = let
  s    = gs ^. stack
  top  = Stack.head s
  s'   = Stack.pop s in
    gs & states %~ Map.delete top
       & stack  .~ s'

safeGetState :: GlobalState -> Maybe State
safeGetState gs = do
  key <- safeHead (gs ^. stack)
  Map.lookup key (gs ^. states)