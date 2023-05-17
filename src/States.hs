{-# LANGUAGE TemplateHaskell #-}
module States (module States, GenIO) where

import Brick (Widget, EventM)
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

data Name = 
          -- Settings
            HintsField
          | ControlsField
          | CaseSensitiveField
          | ShuffleAnswersField
          | EscapeCodeField
          | MaxRecentsField

          -- Parameters
          | ChunkField1
          | ChunkField2
          | SubsetField
          | ShuffleField
          | ReviewModeField
          | ParametersOkField

          | Ordinary
  deriving (Eq, Ord, Show)
type Event = ()

data Mode  = MainMenu    
           | Settings    
           | Info        
           | CardSelector
           | FileBrowser 
           | Cards
           | Parameter
  deriving (Show, Eq, Ord)

data State = MainMenuState     MMS
           | SettingsState     SS
           | InfoState         IS
           | CardSelectorState CSS
           | FileBrowserState  FBS
           | ParameterState    PS
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
  , _stack      :: Stack Mode
  , _states     :: Map Mode State
  , _parameters :: Parameters
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
defaultCardState MultipleChoice{incorrects = ics} = MultipleChoiceState 
  { _highlighted = 0
  , _number = length ics + 1
  , _tried = M.fromList [(i, False) | i <- [0..length ics]] }
defaultCardState OpenQuestion{perforated=perf} = OpenQuestionState 
  { _gapInput = M.empty
  , _highlighted = 0
  , _number = nGapsInPerforated perf
  , _entered = False
  , _correctGaps = M.fromList [(i, False) | i <- [0..nGapsInPerforated perf - 1]]
  , _failed = False }
defaultCardState MultipleAnswer{options=opts} = MultipleAnswerState 
  { _highlighted = 0
  , _selected = M.fromList [(i, False) | i <- [0..NE.length opts-1]]
  , _entered = False
  , _number = NE.length opts }
defaultCardState Reorder{elements=elts} = ReorderState
  { _highlighted = 0
  , _grabbed = False
  , _order = M.fromList (zip [0..] (NE.toList elts))
  , _entered = False
  , _number = NE.length elts }

data CS = CS
  { _originalCards       :: [Card]     -- the deck as it was parsed
  , _shownCards          :: [Card]     -- the deck after shuffling answers and cards
  , _indexMapping        :: [Int]      -- contains the order that shownCards has wrt originalCards
  , _index               :: Int        -- current card index
  , _nCards              :: Int        -- number of cards
  , _currentCard         :: Card
  , _cardState           :: CardState
  , _showHints           :: Bool
  , _showControls        :: Bool
  , _isCaseSensitive     :: Bool      
  , _reviewMode          :: Bool
  , _correctCards        :: [Int]      -- list of indices of correct cards
  , _popup               :: Maybe (Popup GlobalState CS)
  , _pathToFile          :: FilePath
  }

-- -- Lens for just accessing the cards
-- cards :: Lens' CS [Card]
-- cards = lens (map snd . _cardsAndImages) (\cs cards -> cs {_cardsAndImages = zip (map fst (_cardsAndImages cs)) cards})

-- currentCard :: Lens' CS Card
-- currentCard = lens (snd . _currentCardAndImage) (\cs card -> cs {_currentCardAndImage = (fst (_currentCardAndImage cs), card)})

data Popup s d = Popup
  { drawPopup        :: d -> Widget Name
  , handlePopupEvent :: V.Event -> EventM Name s ()
  , _popupState      :: PopupState
  }

data PopupState = 
    CorrectPopup
      { _popupSelected :: Int }
  | FinalPopup
  | DeckMakerPopup
      { _popupSelected     :: Int
      , _makeDeckIncorrect :: Bool
      , _makeDeckCorrect   :: Bool }
  deriving Eq

newtype MMS = MMS 
  { _l  :: List Name String }

type IS = ()

data Settings = FormState
  { _hints           :: Bool
  , _controls        :: Bool
  , _caseSensitive   :: Bool
  , _shuffleAnswers  :: Bool
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

defaultParameters = Parameters
  { _pShuffle    = False
  , _pSubset     = Nothing
  , _pChunk      = Chunk 1 1
  , _pReviewMode = True
  , _pOk         = False }

data Parameters = Parameters
  { _pShuffle    :: Bool
  , _pSubset     :: Maybe Int
  , _pChunk      :: Chunk
  , _pReviewMode :: Bool
  , _pOk         :: Bool }

data PS = PS
  { _psCards     :: [Card]
  , _psFp        :: FilePath
  , _psForm      :: Form Parameters Event Name
  }

makeLenses ''State
makeLenses ''MMS
makeLenses ''GlobalState
makeLenses ''CardState
makeLenses ''CS
makeLenses ''Settings
makeLenses ''CSS
makeLenses ''FBS
makeLenses ''PS
makeLenses ''Parameters
makeLenses ''Popup
makeLenses ''PopupState