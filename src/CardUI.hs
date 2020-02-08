{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module CardUI (runCardUI) where

import Brick
import Lens.Micro.Platform
import Parser
import Types
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
  | OpenQuestionState
  { _gapInput       :: Map Int String
  , _selectedGap    :: Int
  , _nGaps          :: Int
  }

data State = State
  { _cards          :: [Card]     -- list of flashcards
  , _index          :: Int        -- current card index
  , _nCards         :: Int        -- number of cards
  , _currentCard    :: Card
  , _cardState      :: CardState
  }

makeLenses ''CardState
makeLenses ''State

defaultCardState :: Card -> CardState
defaultCardState Definition{} = DefinitionState { _flipped = False }
defaultCardState (MultipleChoice _ _ ics) = MultipleChoiceState { _selected = 0, _nChoices = length ics + 1, _tried = M.fromList [(i, False) | i <- [0..length ics]]}
defaultCardState (OpenQuestion _ perforated) = OpenQuestionState { _gapInput = M.empty, _selectedGap = 0, _nGaps = nGapsInPerforated perforated }


app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s =  [drawCardUI s <=> drawInfo]

drawInfo :: Widget Name
drawInfo = str "ESC: quit"

drawProgress :: State -> Widget Name
drawProgress s = C.hCenter $ str (show (s^.index + 1) ++ "/" ++ show (s^.nCards))

drawHeader :: String -> Widget Name
drawHeader title = withAttr titleAttr $
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
drawCardUI s = joinBorders $ drawCardBox $ (<=> drawProgress s) $
  case (s ^. cards) !! (s ^. index) of
    Definition title descr -> drawHeader title <=> B.hBorder <=> drawDef s descr
                              
    MultipleChoice question correct others -> drawHeader question <=> B.hBorder <=> drawOptions s (listMultipleChoice correct others)

    OpenQuestion title perforated -> drawHeader title <=> B.hBorder <=> drawPerforated s perforated


applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen predicate action = if predicate then action else id

applyUnless :: Bool -> (a -> a) -> a -> a
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

drawPerforated :: State -> Perforated -> Widget Name
drawPerforated s p = drawSentence s $ perforatedToSentence p

drawSentence :: State -> Sentence -> Widget Name
drawSentence = drawSentence' 0
  where
    drawSentence' _ _ (Normal text)             = str text
    drawSentence' i s (Perforated pre gap post) = case s ^. cardState of
      OpenQuestionState {_gapInput = kvs, _selectedGap=j } -> str pre <+> cursor (withAttr gapAttr (str gap)) <+> drawSentence' (i+1) s post
        where gap = M.findWithDefault "" i kvs
              cursor :: Widget Name -> Widget Name
              cursor = if i == j then showCursor () (Location (length gap, 0)) else id
      _ -> error "impossible"

drawCardBox :: Widget Name -> Widget Name
drawCardBox w = C.center $
                withBorderStyle BS.unicodeRounded $
                B.border $
                withAttr textboxAttr $
                hLimitPercent 60 w

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent ev) = case ev of
  V.EvKey V.KEsc []                -> halt s
  V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt s
  ev -> case s ^. cardState of
    MultipleChoiceState {_selected = i, _nChoices = nChoices} ->
      case ev of
        V.EvKey V.KUp [] -> continue up
        V.EvKey (V.KChar 'k') [] -> continue up
        V.EvKey V.KDown [] -> continue down 
        V.EvKey (V.KChar 'j') [] -> continue down

        V.EvKey V.KEnter [] -> case s ^. currentCard of
          MultipleChoice _ (CorrectOption j _) _ ->
            if i == j
              then next s
              else continue $ s & cardState.tried %~ M.insert i True
          _ -> error "impossible"
        _ -> continue s

      where down = if i < nChoices - 1
                     then s & (cardState.selected) +~ 1
                     else s

            up = if i > 0
                   then s & (cardState.selected) -~ 1
                   else s

    DefinitionState{_flipped = f} ->
      case ev of
        V.EvKey V.KEnter [] -> 
          if f
            then next s 
            else continue $ s & cardState.flipped %~ not
        _ -> continue s
    
    OpenQuestionState {_selectedGap = i, _nGaps = n, _gapInput = kvs} ->
      case ev of
        V.EvKey (V.KChar '\t') [] -> continue $ 
          if i < n - 1
            then s & (cardState.selectedGap) +~ 1
            else s & (cardState.selectedGap) .~ 0
        
        V.EvKey V.KRight [] -> continue $ 
          if i < n - 1
            then s & (cardState.selectedGap) +~ 1
            else s

        V.EvKey V.KLeft [] -> continue $ 
          if i > 0
            then s & (cardState.selectedGap) -~ 1
            else s

        V.EvKey (V.KChar c) [] -> continue $
          s & cardState.gapInput.at i.non "" %~ (++[c])    -- should prob. use snoc list for better efficiency
        V.EvKey V.KEnter [] -> case s ^. currentCard of
          OpenQuestion _ perforated -> if correct then next s else continue s
            where correct :: Bool
                  correct = foldSentenceIndex sent perf sentence
                  sentence = perforatedToSentence perforated
                  sent _ _ = True
                  perf _ gap acc i = acc && gap == M.findWithDefault "" i kvs
                  
          _ -> error "impossible"
        V.EvKey V.KBS [] -> continue $ s & cardState.gapInput.ix i %~ backspace
          where backspace "" = ""
                backspace xs = init xs
        _ -> continue s
handleEvent s _ = continue s
-- handleEvent s (VtyEvent (V.EvKey V.KRight []))              = next s
-- handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') []))         = next s
-- handleEvent s (VtyEvent (V.EvKey V.KLeft  []))              = previous s
      
titleAttr :: AttrName
titleAttr = attrName "title"

textboxAttr :: AttrName
textboxAttr = attrName "textbox"

chosenOptAttr :: AttrName
chosenOptAttr = attrName "chosen option"

hiddenAttr :: AttrName
hiddenAttr = attrName "hidden"

gapAttr :: AttrName
gapAttr = attrName "gap"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (titleAttr, fg V.yellow)
  , (textboxAttr, V.defAttr)
  , (chosenOptAttr, fg V.red)
  , (hiddenAttr, fg V.black)
  , (gapAttr, V.defAttr `V.withStyle` V.underline)
  ]

runCardUI :: String -> IO State
runCardUI input = do
  let cards = case parseCards input of
              Left parseError -> error (show parseError)
              Right result -> result 
  let initialState = State { _cards = cards
                           , _index = 0
                           , _currentCard = head cards
                           , _cardState = defaultCardState (head cards)
                           , _nCards = length cards }
  defaultMain app initialState

next :: State -> EventM Name (Next State)
next s
  | s ^. index + 1 < length (s ^. cards) = continue . updateState $ s & index +~ 1
  | otherwise                            = halt s

previous :: State -> EventM Name (Next State)
previous s | s ^. index > 0 = continue . updateState $ s & index -~ 1
           | otherwise      = continue s

updateState :: State -> State
updateState s =
  let card = (s ^. cards) !! (s ^. index) in s
    & currentCard .~ card
    & cardState .~ defaultCardState card
