{-# LANGUAGE TemplateHaskell #-}
module States (module States, GenIO) where

import Brick (Widget, EventM, Next)
import Brick.Forms (Form)
import Brick.Widgets.FileBrowser
import Brick.Widgets.List (List)
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Lens.Micro.Platform
import System.Random.MWC (GenIO)
import Stack hiding (head)
import Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V

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

data Chunk = Chunk Int Int

instance Show Chunk where
  show (Chunk i n) = show i <> "/" <> show n

instance Read Chunk where
  readsPrec _ input =
    let (i', rest1) = span isDigit input
        i = read i' :: Int
        (c:rest2) = rest1
        (n', rest3) = span isDigit rest2
        n = read n' :: Int
    in [(Chunk i n, rest3) | c `elem` ['/', ' '] && n >= i && i >= 1] 

data GlobalState = GlobalState
  { _mwc        :: GenIO
  , _doShuffle  :: Bool
  , _subset     :: Maybe Int
  , _chunk      :: Chunk
  , _stack      :: Stack Mode
  , _states     :: Map Mode State
  , _doReview   :: Bool
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
  , _failed         :: Bool
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
  , _correctGaps = M.fromList [(i, False) | i <- [0..nGapsInPerforated perforated - 1]]
  , _failed = False }
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
  , _reviewMode     :: Bool
  , _correctCards   :: [Int]      -- list of indices of correct cards
  , _popup          :: Maybe (Popup CS)
  }

data Popup s = Popup 
  { drawPopup        :: s -> Widget Name
  , handlePopupEvent :: GlobalState -> s -> V.Event -> EventM Name (Next GlobalState)
  , _popupState      :: PopupState
  }

data PopupState = 
    CorrectPopup
      { _popupSelected :: Int }
  | FinalPopup
  deriving Eq

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

makeLenses ''State
makeLenses ''MMS
makeLenses ''GlobalState
makeLenses ''CardState
makeLenses ''CS
makeLenses ''Settings
makeLenses ''CSS
makeLenses ''FBS
makeLenses ''Popup
makeLenses ''PopupState
