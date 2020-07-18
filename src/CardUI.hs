{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module CardUI (runCardUI) where

import Brick
import BrickHelpers
import Lens.Micro.Platform
import Types
import Data.Char (isSeparator, isSpace)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Text.Wrap
import Data.Text (pack)
import SettingsUI (getShowHints, getShowControls)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List.NonEmpty as NE
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
  { _highlighted    :: Int
  , _nChoices       :: Int
  , _tried          :: Map Int Bool      -- indices of tried choices
  }
  | MultipleAnswerState
  { _highlighted    :: Int
  , _selected       :: Map Int Bool
  , _nChoices       :: Int
  , _entered        :: Bool
  }
  | OpenQuestionState
  { _gapInput       :: Map Int String
  , _highlighted    :: Int
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
  , _showHints      :: Bool
  , _showControls   :: Bool
  -- , _incorrectCards :: [Int]      -- list of indices of incorrect answers
  }

makeLenses ''CardState
makeLenses ''State

defaultCardState :: Card -> CardState
defaultCardState Definition{} = DefinitionState { _flipped = False }
defaultCardState (MultipleChoice _ _ ics) = MultipleChoiceState 
  { _highlighted = 0
  , _nChoices = length ics + 1
  , _tried = M.fromList [(i, False) | i <- [0..length ics]] }
defaultCardState (OpenQuestion _ perforated) = OpenQuestionState 
  { _gapInput = M.empty
  , _highlighted = 0
  , _nGaps = nGapsInPerforated perforated
  , _entered = False
  , _correctGaps = M.fromList [(i, False) | i <- [0..nGapsInPerforated perforated - 1]] }
defaultCardState (MultipleAnswer question answers) = MultipleAnswerState 
  { _highlighted = 0
  , _selected = M.fromList [(i, False) | i <- [0..NE.length answers-1]]
  , _entered = False
  , _nChoices = NE.length answers }

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s =  [drawCardUI s <=> drawInfo s]

drawInfo :: State -> Widget Name
drawInfo s = if not (s ^. showControls) then emptyWidget else
  strWrap . ("ESC: quit" <>) $ case s ^. cardState of
    DefinitionState {}     -> ", ENTER: flip card / continue"
    MultipleChoiceState {} -> ", ENTER: confirm answer / continue"
    MultipleAnswerState {} -> ", ENTER: select / continue, c: confirm selection"
    OpenQuestionState {}   -> ", LEFT/RIGHT/TAB: navigate gaps, ENTER: confirm answer / continue"

drawProgress :: State -> Widget Name
drawProgress s = C.hCenter $ str (show (s^.index + 1) ++ "/" ++ show (s^.nCards))

drawHeader :: String -> Widget Name
drawHeader title = withAttr titleAttr $
                   padLeftRight 1 $
                   hCenteredStrWrap title

wrapSettings :: WrapSettings
wrapSettings = WrapSettings {preserveIndentation=False, breakLongWords=True}

drawDescr :: String -> Widget Name
drawDescr descr =
  strWrapWith wrapSettings descr'
    where
      descr' = dropWhileEnd isSpace descr

listMultipleChoice :: CorrectOption -> [IncorrectOption] -> [String]
listMultipleChoice c = reverse . listMultipleChoice' [] 0 c
  where listMultipleChoice' opts i (CorrectOption j cStr) [] = 
          if i == j
            then cStr : opts
            else opts
        listMultipleChoice' opts i c'@(CorrectOption j cStr) ics@(IncorrectOption icStr : ics') = 
          if i == j
            then listMultipleChoice' (cStr  : opts) (i+1) c' ics
            else listMultipleChoice' (icStr : opts) (i+1) c' ics'

drawCardUI :: State -> Widget Name
drawCardUI s = let p = 1 in
  joinBorders $ drawCardBox $ (<=> drawProgress s) $
  case (s ^. cards) !! (s ^. index) of
    Definition title descr -> drawHeader title <=> B.hBorder <=> padLeftRight p (drawDef s descr <=> str " ")
                              
    MultipleChoice question correct others -> drawHeader question <=> B.hBorder <=> padLeftRight p (drawChoices s (listMultipleChoice correct others) <=> str " ")

    OpenQuestion title perforated -> drawHeader title <=> B.hBorder <=> padLeftRight p (drawPerforated s perforated <=> str " ")

    MultipleAnswer question options -> drawHeader question <=> B.hBorder <=> padRight (Pad p) (drawOptions s options <=> str " ")

drawDef :: State -> String -> Widget Name
drawDef s def = if s ^. showHints then drawHintedDef s def else drawNormalDef s def

drawHintedDef :: State -> String -> Widget Name
drawHintedDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f then drawDescr def else drawDescr [if isSeparator char || char == '\n' then char else '_' | char <- def]
  _ -> error "impossible: " 

drawNormalDef:: State -> String -> Widget Name
drawNormalDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f
    then drawDescr def
    else Widget Greedy Fixed $ do
      c <- getContext
      let w = c^.availWidthL
      let def' = dropWhileEnd isSpace def
      render . vBox $ [str " " | _ <- wrapTextToLines wrapSettings w (pack def')]
  _ -> error "impossible: " 

drawChoices :: State -> [String] -> Widget Name
drawChoices s options = case (s ^. cardState, s ^. currentCard) of
  (MultipleChoiceState {_highlighted=i, _tried=kvs}, MultipleChoice _ (CorrectOption k _) _)  -> vBox formattedOptions
                  
             where formattedOptions :: [Widget Name]
                   formattedOptions = [ prefix <+> coloring (drawDescr opt) |
                                        (j, opt) <- zip [0..] options,
                                        let prefix = if i == j then withAttr highlightedChoiceAttr (str "* ") else str "  "
                                            chosen = M.findWithDefault False j kvs 
                                            coloring = case (chosen, j==k) of
                                              (False, _)    -> id
                                              (True, False) -> withAttr incorrectChoiceAttr
                                              (True, True)  -> withAttr correctChoiceAttr
                                          ]
  _ -> error "impossible"

drawOptions :: State -> NonEmpty Option -> Widget Name
drawOptions s = case (s ^. cardState, s ^. currentCard) of
  (MultipleAnswerState {_highlighted=j, _selected=kvs, _entered=submitted}, _) -> 
    vBox . NE.toList .  NE.map drawOption . (`NE.zip` NE.fromList [0..])
      where drawOption (Option kind text, i) = coloring (str "[") <+> coloring (highlighting (str symbol)) <+> coloring (str "] ") <+> drawDescr text
                where symbol = if (i == j && not submitted) || enabled then "*" else " "
                      enabled = M.findWithDefault False i kvs
                      highlighting = if i == j && not submitted then withAttr highlightedOptAttr else id
                      coloring = case (submitted, enabled, kind) of
                                   (True, True, Correct) -> withAttr correctOptAttr
                                   (True, False, Incorrect) -> withAttr correctOptAttr
                                   (True, _, _) -> withAttr incorrectOptAttr
                                   (False, True, _) -> withAttr selectedOptAttr
                                   _ -> id

  _ -> error "hopefully this is never shown"


drawPerforated :: State -> Perforated -> Widget Name
drawPerforated s p = drawSentence s $ perforatedToSentence p

drawSentence :: State -> Sentence -> Widget Name
drawSentence state sentence = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  render $ makeSentenceWidget w state sentence

makeSentenceWidget :: Int -> State -> Sentence -> Widget Name
makeSentenceWidget w state = vBox . fst . makeSentenceWidget' 0 0
  where
    makeSentenceWidget' :: Int -> Int -> Sentence -> ([Widget Name], Bool)
    makeSentenceWidget' padding _ (Normal s) = let (ws, _, fit) = wrapStringWithPadding padding w s in (ws, fit) 
    makeSentenceWidget' padding i (Perforated pre _ post) = case state ^. cardState of
      OpenQuestionState {_gapInput = kvs, _highlighted=j, _entered=submitted, _correctGaps=cgs} ->
        let (ws, n, fit') = wrapStringWithPadding padding w pre
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
                then let (ws1@(w':ws'), fit) = makeSentenceWidget' (w-n') (i+1) post in
                  if fit then ((ws & _last %~ (<+> (gapWidget <+> w'))) ++ ws', fit')
                  else ((ws & _last %~ (<+> gapWidget)) ++ ws1, fit')
              else let (ws1@(w':ws'), fit) = makeSentenceWidget' (length gap) (i+1) post in
                if fit then (ws ++ [gapWidget <+> w'] ++ ws', fit')
                else (ws ++ [gapWidget] ++ ws1, fit')
      _ -> error "PANIC!"

wrapStringWithPadding :: Int -> Int -> String -> ([Widget Name], Int, Bool)
wrapStringWithPadding padding w s
  | null (words s) = ([str ""], padding, True)
  | otherwise = if length (head (words s)) < w - padding then
    let startsWithSpace = head s == ' ' 
        s' = if startsWithSpace then " " <> replicate padding 'X' <> tail s else replicate padding 'X' ++ s
        lastLetter = last s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines wrapSettings w (pack s') & ix 0 %~ (if startsWithSpace then (T.pack " " `T.append`) . T.drop (padding + 1) else T.drop padding)
        ts' = ts & _last %~ (`T.append` postfix)
        padding' = T.length (last ts') + (if length ts' == 1 then 1 else 0) * padding in
          (map txt (filter (/=T.empty) ts'), padding', True)
  else
    let lastLetter = last s
        (x: xs) = s
        s' = if x == ' ' then xs else s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines wrapSettings w (pack s')
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
handleEvent s (VtyEvent e) = case e of
  V.EvKey V.KEsc []                -> halt s
  V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt s
  V.EvKey V.KRight [V.MCtrl]       -> next s
  V.EvKey V.KLeft  [V.MCtrl]       -> previous s
  -- V.EvKey (V.KChar ' ') []         -> next s

  ev -> case (s ^. cardState, s ^. currentCard) of
    (DefinitionState{_flipped = f}, _) ->
      case ev of
        V.EvKey V.KEnter [] -> 
          if f
            then next s 
            else continue $ s & cardState.flipped %~ not
        _ -> continue s

    (MultipleChoiceState {_highlighted = i, _nChoices = n, _tried = kvs}, MultipleChoice _ (CorrectOption j _) _) ->
      case ev of
        V.EvKey V.KUp [] -> continue up
        V.EvKey (V.KChar 'k') [] -> continue up
        V.EvKey V.KDown [] -> continue down 
        V.EvKey (V.KChar 'j') [] -> continue down

        V.EvKey V.KEnter [] ->
            if frozen
              then next s
              else continue $ s & cardState.tried %~ M.insert i True

        _ -> continue s

      where frozen = M.findWithDefault False j kvs
        
            down = if i < n - 1 && not frozen
                     then s & (cardState.highlighted) +~ 1
                     else s

            up = if i > 0 && not frozen
                   then s & (cardState.highlighted) -~ 1
                   else s
    
    (MultipleAnswerState {_highlighted = i, _nChoices = n, _entered = submitted}, MultipleAnswer {}) ->
      case ev of
        V.EvKey V.KUp [] -> continue up
        V.EvKey (V.KChar 'k') [] -> continue up
        V.EvKey V.KDown [] -> continue down 
        V.EvKey (V.KChar 'j') [] -> continue down

        V.EvKey (V.KChar 'c') [] -> continue $ s & (cardState.entered) .~ True

        V.EvKey V.KEnter [] ->
            if frozen
              then next s
              else continue $ s & cardState.selected %~ M.adjust not i

        _ -> continue s


      where frozen = submitted
        
            down = if i < n - 1 && not frozen
                     then s & (cardState.highlighted) +~ 1
                     else s

            up = if i > 0 && not frozen
                   then s & (cardState.highlighted) -~ 1
                   else s

    (OpenQuestionState {_highlighted = i, _nGaps = n, _gapInput = kvs, _correctGaps = cGaps}, OpenQuestion _ perforated) ->
      let correct = M.foldr (&&) True cGaps in
        case ev of
          V.EvKey (V.KChar '\t') [] -> continue $ 
            if i < n - 1 && not correct
              then s & (cardState.highlighted) +~ 1
              else s & (cardState.highlighted) .~ 0
          
          V.EvKey V.KRight [] -> continue $ 
            if i < n - 1 && not correct
              then s & (cardState.highlighted) +~ 1
              else s

          V.EvKey V.KLeft [] -> continue $ 
            if i > 0 && not correct
              then s & (cardState.highlighted) -~ 1
              else s

          V.EvKey (V.KChar c) [] -> continue $
            if correct then s else s & cardState.gapInput.at i.non "" %~ (++[c])

          V.EvKey V.KEnter [] -> if correct then next s else continue s'
              where sentence = perforatedToSentence perforated
                    gaps = sentenceToGaps sentence

                    s' = s & (cardState.correctGaps) %~ M.mapWithKey (\j _ -> M.findWithDefault "" j kvs `elem` gaps !! j)  & (cardState.entered) .~ True

          V.EvKey V.KBS [] -> continue $ 
              if correct then s else s & cardState.gapInput.ix i %~ backspace
            where backspace "" = ""
                  backspace xs = init xs
          _ -> continue s
      
    _ -> error "impossible"
handleEvent s _ = continue s
      
titleAttr :: AttrName
titleAttr = attrName "title"

textboxAttr :: AttrName
textboxAttr = attrName "textbox"

highlightedChoiceAttr :: AttrName
highlightedChoiceAttr = attrName "highlighted choice"

incorrectChoiceAttr :: AttrName
incorrectChoiceAttr = attrName "incorrect choice"

correctChoiceAttr :: AttrName
correctChoiceAttr = attrName "correct choice"

highlightedOptAttr :: AttrName
highlightedOptAttr = attrName "highlighted option"

selectedOptAttr :: AttrName
selectedOptAttr = attrName "selected option"

correctOptAttr :: AttrName
correctOptAttr = attrName "correct option"

incorrectOptAttr :: AttrName
incorrectOptAttr = attrName "incorrect option"

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
  , (highlightedChoiceAttr, fg V.yellow)
  , (incorrectChoiceAttr, fg V.red)
  , (correctChoiceAttr, fg V.green)
  , (incorrectGapAttr, fg V.red `V.withStyle` V.underline)
  , (correctGapAttr, fg V.green `V.withStyle` V.underline)
  , (highlightedOptAttr, fg V.yellow)
  , (selectedOptAttr, fg V.blue)
  , (incorrectOptAttr, fg V.red)
  , (correctOptAttr, fg V.green)
  , (hiddenAttr, fg V.black)
  , (gapAttr, V.defAttr `V.withStyle` V.underline)
  ]

runCardUI :: [Card] -> IO State
runCardUI deck = do
  hints <- getShowHints
  controls <- getShowControls

  let initialState = State { _cards = deck
                           , _index = 0
                           , _currentCard = head deck
                           , _cardState = defaultCardState (head deck)
                           , _nCards = length deck
                           , _showHints = hints
                           , _showControls = controls }
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
