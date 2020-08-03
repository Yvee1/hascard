{-# LANGUAGE TemplateHaskell #-}
module States where

import Brick
import Brick.Forms (Form)
import Brick.Widgets.FileBrowser
import Brick.Widgets.List (List)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Lens.Micro.Platform
import System.Random.MWC (GenIO)
import Stack hiding (head)
import Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Stack

data Name = HintsField
          | ControlsField
          | EscapeCodeField
          | MaxRecentsField
          | Ordinary
  deriving (Eq, Ord, Show)
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

defaultCardState :: Card -> CardState
defaultCardState Definition{} = DefinitionState { _flipped = False }
defaultCardState (MultipleChoice _ _ ics) = MultipleChoiceState 
  { _highlighted = 0
  , _number = length ics + 1
  , _tried = M.fromList [(i, False) | i <- [0..length ics]] }
defaultCardState (OpenQuestion _ perforated) = OpenQuestionState 
  { _gapInput = M.empty
  , _highlighted = 0
  , _number = nGapsInPerforated perforated
  , _entered = False
  , _correctGaps = M.fromList [(i, False) | i <- [0..nGapsInPerforated perforated - 1]] }
defaultCardState (MultipleAnswer _ answers) = MultipleAnswerState 
  { _highlighted = 0
  , _selected = M.fromList [(i, False) | i <- [0..NE.length answers-1]]
  , _entered = False
  , _number = NE.length answers }
defaultCardState (Reorder _ elements) = ReorderState
  { _highlighted = 0
  , _grabbed = False
  , _order = M.fromList (zip [0..] (NE.toList elements))
  , _entered = False
  , _number = NE.length elements }

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

data Settings = FormState
  { _hints           :: Bool
  , _controls        :: Bool
  , _escapeCode      :: Bool
  , _maxRecents      :: Int }
  deriving (Read, Show)

type SS = Form Settings Event Name

data CSS = CSS
  { _list             :: List Name String
  , _exception        :: Maybe String
  , _recents          :: Stack FilePath
  , _maxRecentsToShow :: Int
  }

data FBS = FBS
  { _fb          :: FileBrowser Name
  , _exception'  :: Maybe String
  , _parsedCards :: [Card]
  , _filePath    :: Maybe FilePath
  , _showHidden  :: Bool
  }

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
makeLenses ''Settings
makeLenses ''CSS
makeLenses ''FBS

getState :: GlobalState -> State
getState = fromJust . safeGetState

updateState :: GlobalState -> State -> GlobalState
updateState gs s = gs & states %~ M.insert (getMode s) s

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
goToState gs s = gs & states %~ M.insert (getMode s) s
                    & stack  %~ insert (getMode s)

moveToState :: GlobalState -> State -> GlobalState 
moveToState gs = goToState (popState gs)

popState :: GlobalState -> GlobalState
popState gs = let
  s    = gs ^. stack
  top  = Stack.head s
  s'   = Stack.pop s in
    gs & states %~ M.delete top
       & stack  .~ s'

safeGetState :: GlobalState -> Maybe State
safeGetState gs = do
  key <- safeHead (gs ^. stack)
  M.lookup key (gs ^. states)

goToModeOrQuit :: GlobalState -> Mode -> EventM n (Next GlobalState)
goToModeOrQuit gs mode = 
  maybe (halt gs) (continue . goToState gs) $ M.lookup mode (gs ^. states) 

moveToModeOrQuit :: GlobalState -> Mode -> EventM n (Next GlobalState)
moveToModeOrQuit gs mode = 
  maybe (halt gs) (continue . moveToState gs) $ M.lookup mode (gs ^. states) 