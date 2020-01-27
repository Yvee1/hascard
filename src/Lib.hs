{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Data.Char
import Brick
import Lens.Micro.Platform
import Parser
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

type Event = ()
type Name = ()

data CardState = 
    DefinitionState
  { _flipped        :: Bool }
  | MultipleChoiceState
  { _selected       :: Int
  , _nChoices       :: Int
  , _tried          :: Map Int Bool      -- indices of tried choices
  }

data State = State
  { _cards          :: [Card]     -- list of flashcards
  , _index          :: Int        -- current card index
  , _currentCard    :: Card
  , _cardState      :: CardState
  }

makeLenses ''CardState
makeLenses ''State

defaultCardState :: Card -> CardState
defaultCardState Definition{} = DefinitionState { _flipped = False }
defaultCardState (MultipleChoice _ _ ics) = MultipleChoiceState { _selected = 0, _nChoices = length ics + 1, _tried = M.fromList [(i, False) | i <- [0..length ics]]}

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s =  [drawCardUI s]

drawHeader :: String -> Widget Name
drawHeader title = padBottom (Pad 1) $
                   withAttr titleAttr $
                   padLeftRight 1 $
                   C.hCenter (strWrap title)

drawDescr :: String -> Widget Name
drawDescr descr = padLeftRight 1 $
                  strWrap descr

listMultipleChoice :: CorrectOption -> [IncorrectOption] -> [String]
listMultipleChoice c = reverse . listMultipleChoice' [] 0 c
  where listMultipleChoice' opts i c@(CorrectOption j cStr) [] = 
          if i == j
            then cStr : opts
            else opts
        listMultipleChoice' opts i c@(CorrectOption j cStr) ics@(IncorrectOption icStr : ics') = 
          if i == j
            then listMultipleChoice' (cStr  : opts) (i+1) c ics
            else listMultipleChoice' (icStr : opts) (i+1) c ics'

drawCardUI :: State -> Widget Name
drawCardUI s = 
  case (s ^. cards) !! (s ^. index) of
    Definition title descr -> drawCardBox $
                              drawHeader title <=> drawDef s descr
                              
    MultipleChoice question correct others -> drawCardBox $
                                              drawHeader question <=> drawOptions s (listMultipleChoice correct others)

applyWhen predicate action = if predicate then action else id
applyUnless p = applyWhen (not p)

drawDef :: State -> String -> Widget Name
drawDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> applyUnless f (withAttr hiddenAttr) $ drawDescr def
    
  _ -> error "impossible: " 

drawOptions :: State -> [String] -> Widget Name
drawOptions s options = case s ^. cardState of
  MultipleChoiceState {_selected=i, _tried=kvs}  -> vBox formattedOptions
                  
             where formattedOptions :: [Widget Name]
                   formattedOptions = [ (if chosen then withAttr chosenOptAttr else id) $ drawDescr (if i==j then "* " ++ opt else opt) |
                                        (j, opt) <- zip [0..] options,
                                        let chosen = M.findWithDefault False j kvs ]

  _                                  -> error "impossible"

drawCardBox :: Widget Name -> Widget Name
drawCardBox w = C.center $
                withBorderStyle BS.unicodeRounded $
                B.border $
                withAttr textboxAttr $
                hLimitPercent 60 w

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') []))         = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))                = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  = halt s
handleEvent s (VtyEvent (V.EvKey V.KRight []))              = next s
handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') []))         = next s
handleEvent s (VtyEvent (V.EvKey V.KLeft  []))              = previous s
handleEvent s (VtyEvent (V.EvKey V.KUp []))                 = continue $
  case s ^. cardState of
    MultipleChoiceState {_selected=i} -> if i > 0 then s & (cardState.selected) -~ 1 else s
    _ -> s

handleEvent s (VtyEvent (V.EvKey V.KDown []))               = continue $
  case s ^. cardState of
    MultipleChoiceState {_selected=i, _nChoices = nChoices} ->
      if i < nChoices - 1
        then s & (cardState.selected) +~ 1
        else s
    _ -> s

handleEvent s (VtyEvent (V.EvKey V.KEnter []))              =
  case s ^. cardState of
    MultipleChoiceState {_selected=i} ->
      case s ^. currentCard of
        MultipleChoice _ (CorrectOption j _) _ ->
          if i == j
            then next s
            else continue $ s & cardState.tried %~ M.insert i True
        _ -> error "impossible"

    DefinitionState{} -> continue $ s & cardState.flipped %~ not
handleEvent s _                                             = continue s
  
titleAttr :: AttrName
titleAttr = attrName "title"

textboxAttr :: AttrName
textboxAttr = attrName "textbox"

chosenOptAttr :: AttrName
chosenOptAttr = attrName "chosen option"

hiddenAttr :: AttrName
hiddenAttr = attrName "hidden"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (titleAttr, bg V.green `V.withStyle` V.bold `V.withStyle` V.underline)
  , (textboxAttr, V.defAttr)
  , (chosenOptAttr, fg V.red)
  , (hiddenAttr, fg V.black)
  ]
 
handleFilePath :: FilePath -> IO String
handleFilePath = readFile

runBrickFlashcards :: String -> IO ()
runBrickFlashcards input = do
  let cards = case parseCards input of
              Left parseError -> error (show parseError)
              Right result -> result 
  let initialState = State { _cards = cards
                           , _index = 0
                           , _currentCard = head cards
                           , _cardState = defaultCardState (head cards)}
  finalState <- defaultMain app initialState
  pure ()

next :: State -> EventM Name (Next State)
next s
  | s ^. index + 1 < length (s ^. cards) = continue . updateState $ s & index +~ 1
  | otherwise                            = continue s

previous :: State -> EventM Name (Next State)
previous s | s ^. index > 0 = continue . updateState $ s & index -~ 1
           | otherwise      = continue s

updateState :: State -> State
updateState s =
  let card = (s ^. cards) !! (s ^. index) in s
    & currentCard .~ card
    & cardState .~ defaultCardState card
