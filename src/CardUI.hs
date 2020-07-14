{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module CardUI (runCardUI) where

import Brick
import BrickHelpers
import Lens.Micro.Platform
import Parser
import Types
import Data.Char (isSeparator)
import Data.Map.Strict (Map)
import Text.Wrap
import Data.Text (pack)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
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
  , _entered        :: Bool
  , _correctGaps    :: Map Int Bool
  }

data State = State
  { _cards          :: [Card]     -- list of flashcards
  , _index          :: Int        -- current card index
  , _nCards         :: Int        -- number of cards
  , _currentCard    :: Card
  , _cardState      :: CardState
  , _incorrectCards :: [Int]      -- list of indices of incorrect answers
  }

makeLenses ''CardState
makeLenses ''State

defaultCardState :: Card -> CardState
defaultCardState Definition{} = DefinitionState { _flipped = False }
defaultCardState (MultipleChoice _ _ ics) = MultipleChoiceState 
  { _selected = 0
  , _nChoices = length ics + 1
  , _tried = M.fromList [(i, False) | i <- [0..length ics]]}
defaultCardState (OpenQuestion _ perforated) = OpenQuestionState 
  { _gapInput = M.empty
  , _selectedGap = 0
  , _nGaps = nGapsInPerforated perforated
  , _entered = False
  , _correctGaps = M.fromList [(i, False) | i <- [0..nGapsInPerforated perforated - 1]] }


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
                   hCenteredStrWrap title

drawDescr :: String -> Widget Name
drawDescr descr = padLeftRight 1 $
                  strWrapWith (WrapSettings {preserveIndentation=False, breakLongWords=True}) descr

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

    OpenQuestion title perforated -> drawHeader title <=> B.hBorder <=> padLeftRight 1 (drawPerforated s perforated <=> str " ")


applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen predicate action = if predicate then action else id

applyUnless :: Bool -> (a -> a) -> a -> a
applyUnless p = applyWhen (not p)

drawHintedDef :: State -> String -> Widget Name
drawHintedDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f then drawDescr def else drawDescr [if isSeparator char || char == '\n' then char else '_' | char <- def]
  _ -> error "impossible: " 

drawDef:: State -> String -> Widget Name
drawDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f then drawDescr def else drawDescr [if char == '\n' then char else ' ' | char <- def]
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
drawSentence state sentence = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  render $ helper2 w state sentence

helper2 :: Int -> State -> Sentence -> Widget Name
helper2 w state = vBox . fst . helper2' 0 0
  where
    helper2' :: Int -> Int -> Sentence -> ([Widget Name], Bool)
    helper2' padding _ (Normal s) = let (ws, _, fit) = helper padding w s in (ws, fit) 
    helper2' padding i (Perforated pre gapSolution post) = case state ^. cardState of
      OpenQuestionState {_gapInput = kvs, _selectedGap=j, _entered=submitted, _correctGaps=cgs} ->
        let (ws, n, fit') = helper padding w pre
            gap = M.findWithDefault "" i kvs
            n' =  w - n - length gap 

            cursor :: Widget Name -> Widget Name
            -- i is the index of the gap that we are drawing; j is the gap that is currently selected
            cursor = if i == j then showCursor () (Location (length gap, 0)) else id

            correct = M.findWithDefault False i cgs
            coloring = case (submitted, correct) of
              (False, _) -> withAttr gapAttr
              (True, False) -> withAttr incorrectGapAttr
              (True, True) -> withAttr correctGapAttr
              
            gapWidget = cursor $ coloring (str gap) in

              if n' >= 0 
                then let (ws1@(w':ws'), fit) = helper2' (w-n') (i+1) post in
                  if fit then ((ws & _last %~ (<+> (gapWidget <+> w'))) ++ ws', fit')
                  else ((ws & _last %~ (<+> gapWidget)) ++ ws1, fit')
              else let (ws1@(w':ws'), fit) = helper2' (length gap) (i+1) post in
                if fit then (ws ++ [gapWidget <+> w'] ++ ws', fit')
                else (ws ++ [gapWidget] ++ ws1, fit')
      _ -> error "PANIC!"

helper :: Int -> Int -> String -> ([Widget Name], Int, Bool)
helper padding w s = if words s == [] then ([str ""], padding, True) else
  if length (head (words s)) < w - padding then
    let startsWithSpace = head s == ' ' 
        s' = if startsWithSpace then " " <> replicate padding 'X' <> tail s else replicate padding 'X' ++ s
        lastLetter = last s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines defaultWrapSettings w (pack s') & ix 0 %~ (if startsWithSpace then (T.pack " " `T.append`) . T.drop (padding + 1) else T.drop padding)
        ts' = ts & _last %~ (`T.append` postfix)
        padding' = T.length (last ts') + (if length ts' == 1 then 1 else 0) * padding in
          (map txt (filter (/=T.empty) ts'), padding', True)
  else
    let lastLetter = last s
        (x: xs) = s
        s' = if x == ' ' then xs else s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines defaultWrapSettings w (pack s')
        ts' = ts & _last %~ (`T.append` postfix) in
    (map txt (filter (/=T.empty) ts'), T.length (last ts'), False)

debugToFile :: String -> a -> a
debugToFile s expr = unsafePerformIO $ do
  appendFile "log.txt" s
  return expr

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
  V.EvKey V.KRight []              -> next s
  V.EvKey V.KLeft  []              -> previous s
  -- V.EvKey (V.KChar ' ') []         -> next s
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
    
    OpenQuestionState {_selectedGap = i, _nGaps = n, _gapInput = kvs, _correctGaps = cGaps} ->
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
          OpenQuestion _ perforated -> if correct then next s else continue s'
            where correct :: Bool
                  sentence = perforatedToSentence perforated
                  gaps = sentenceToGaps sentence

                  s' = s & (cardState.correctGaps) %~ M.mapWithKey (\i _ -> gaps !! i == M.findWithDefault "" i kvs) & (cardState.entered) .~ True
                  correct = M.foldr (&&) True (s' ^. (cardState.correctGaps))
                  
          _ -> error "impossible"
        V.EvKey V.KBS [] -> continue $ s & cardState.gapInput.ix i %~ backspace
          where backspace "" = ""
                backspace xs = init xs
        _ -> continue s
handleEvent s _ = continue s
      
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

incorrectGapAttr :: AttrName
incorrectGapAttr = attrName "incorrect gap"

correctGapAttr :: AttrName
correctGapAttr = attrName "correct gap"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (titleAttr, fg V.yellow)
  , (textboxAttr, V.defAttr)
  , (chosenOptAttr, fg V.red)
  , (incorrectGapAttr, fg V.red `V.withStyle` V.underline)
  , (correctGapAttr, fg V.green `V.withStyle` V.underline)
  , (hiddenAttr, fg V.black)
  , (gapAttr, V.defAttr `V.withStyle` V.underline)
  ]

runCardUI :: [Card] -> IO State
runCardUI cards = do
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
