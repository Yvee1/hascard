module UI.Cards (Card, State(..), drawUI, handleEvent, theMap) where

import Brick
import Lens.Micro.Platform
import Types
import States
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Text.Wrap
import Data.Text (pack)
import DeckHandling
import UI.Attributes
import UI.BrickHelpers
import Settings
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

---------------------------------------------------
--------------------- DRAWING ---------------------
---------------------------------------------------

drawUI :: CS -> [Widget Name]
drawUI s =  [drawCardUI s <=> drawInfo s]

drawInfo :: CS -> Widget Name
drawInfo s = if not (s ^. showControls) then emptyWidget else
  strWrap . ("ESC: quit" <>) $ case s ^. cardState of
    DefinitionState {}     -> ", ENTER: flip card / continue"
    MultipleChoiceState {} -> ", ENTER: submit answer / continue"
    MultipleAnswerState {} -> ", ENTER: select / continue, c: submit selection"
    OpenQuestionState {}   -> ", LEFT/RIGHT/TAB: navigate gaps, ENTER: submit answer / continue, F1: show answer"
    ReorderState {}        -> ", ENTER: grab, c: submit answer"

drawCardBox :: Widget Name -> Widget Name
drawCardBox w = C.center $
                withBorderStyle BS.unicodeRounded $
                B.border $
                withAttr textboxAttr $
                hLimitPercent 60 w

drawProgress :: CS -> Widget Name
drawProgress s = C.hCenter $ str (show (s^.index + 1) ++ "/" ++ show (s^.nCards))

drawCardUI :: CS -> Widget Name
drawCardUI s = let p = 1 in
  joinBorders $ drawCardBox $ (<=> drawProgress s) $
  case (s ^. cards) !! (s ^. index) of
    Definition title descr -> drawHeader title
                          <=> B.hBorder
                          <=> padLeftRight p (drawDef s descr <=> str " ")
                              
    MultipleChoice question correct others -> drawHeader question 
                                          <=> B.hBorder 
                                          <=> padLeftRight p (drawChoices s (listMultipleChoice correct others) <=> str " ")

    OpenQuestion title perforated -> drawHeader title
                                 <=> B.hBorder
                                 <=> padLeftRight p (drawPerforated s perforated <=> str " ")

    MultipleAnswer question options -> drawHeader question
                                   <=> B.hBorder
                                   <=> padRight (Pad p) (drawOptions s options <=> str " ")

    Reorder question elements -> drawHeader question
                             <=> B.hBorder
                             <=> padLeftRight p (drawReorder s elements <=> str " ")

drawHeader :: String -> Widget Name
drawHeader title = withAttr titleAttr $
                   padLeftRight 1 $
                   hCenteredStrWrap title

wrapSettings :: WrapSettings
wrapSettings = WrapSettings {preserveIndentation=False, breakLongWords=True}

isSpace' :: Char -> Bool
isSpace' '\r' = True
isSpace' a    = isSpace a

drawDescr :: String -> Widget Name
drawDescr descr =
  strWrapWith wrapSettings descr'
    where
      descr' = dropWhileEnd isSpace' descr

drawDef :: CS -> String -> Widget Name
drawDef s def = if s ^. showHints then drawHintedDef s def else drawNormalDef s def

drawHintedDef :: CS -> String -> Widget Name
drawHintedDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f then drawDescr def else drawDescr [if isSpace' char then char else '_' | char <- def]
  _ -> error "impossible: " 

drawNormalDef:: CS -> String -> Widget Name
drawNormalDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f
    then drawDescr def
    else Widget Greedy Fixed $ do
      c <- getContext
      let w = c^.availWidthL
      let def' = dropWhileEnd isSpace' def
      render . vBox $ [str " " | _ <- wrapTextToLines wrapSettings w (pack def')]
  _ -> error "impossible: " 

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

drawChoices :: CS -> [String] -> Widget Name
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

drawOptions :: CS -> NonEmpty Option -> Widget Name
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


drawPerforated :: CS -> Perforated -> Widget Name
drawPerforated s p = drawSentence s $ perforatedToSentence p

drawSentence :: CS -> Sentence -> Widget Name
drawSentence state sentence = Widget Greedy Fixed $ do
  c <- getContext
  let w = c^.availWidthL
  render $ makeSentenceWidget w state sentence

makeSentenceWidget :: Int -> CS -> Sentence -> Widget Name
makeSentenceWidget w state = vBox . fst . makeSentenceWidget' 0 0
  where
    makeSentenceWidget' :: Int -> Int -> Sentence -> ([Widget Name], Bool)
    makeSentenceWidget' padding _ (Normal s) = let (ws, _, fit) = wrapStringWithPadding padding w s in (ws, fit) 
    makeSentenceWidget' padding i (Perforated pre _ post) = case state ^. cardState of
      OpenQuestionState {_gapInput = kvs, _highlighted=j, _entered=submitted, _correctGaps=cgs} ->
        let (ws, n, fit') = wrapStringWithPadding padding w pre
            gap = M.findWithDefault "" i kvs
            n' =  w - n - textWidth gap 

            cursor :: Widget Name -> Widget Name
            -- i is the index of the gap that we are drawing; j is the gap that is currently selected
            cursor = if i == j then showCursor () (Location (textWidth gap, 0)) else id

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
              else let (ws1@(w':ws'), fit) = makeSentenceWidget' (textWidth gap) (i+1) post in
                if fit then (ws ++ [gapWidget <+> w'] ++ ws', fit')
                else (ws ++ [gapWidget] ++ ws1, fit')
      _ -> error "PANIC!"

wrapStringWithPadding :: Int -> Int -> String -> ([Widget Name], Int, Bool)
wrapStringWithPadding padding w s
  | null (words s) = ([str ""], padding, True)
  | otherwise = if textWidth (head (words s)) < w - padding then
    let startsWithSpace = head s == ' ' 
        s' = if startsWithSpace then " " <> replicate padding 'X' <> tail s else replicate padding 'X' ++ s
        lastLetter = last s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines wrapSettings w (pack s') & ix 0 %~ (if startsWithSpace then (T.pack " " `T.append`) . T.drop (padding + 1) else T.drop padding)
        ts' = ts & _last %~ (`T.append` postfix)
        padding' = textWidth (last ts') + (if length ts' == 1 then 1 else 0) * padding in
          (map txt (filter (/=T.empty) ts'), padding', True)
  else
    let lastLetter = last s
        (x: xs) = s
        s' = if x == ' ' then xs else s
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines wrapSettings w (pack s')
        ts' = ts & _last %~ (`T.append` postfix) in
    (map txt (filter (/=T.empty) ts'), textWidth (last ts'), False)

drawReorder :: CS -> NonEmpty (Int, String) -> Widget Name
drawReorder s elements = case (s ^. cardState, s ^. currentCard) of
  (ReorderState {_highlighted=j, _grabbed=g, _order=kvs, _number=n, _entered=submitted}, Reorder _ _) -> 
    vBox . flip map (map (\i -> (i, kvs M.! i)) [0..n-1]) $
    \(i, (k, text)) ->
      let color = case (i == j,  g) of
                  (True, True ) -> withAttr grabbedElementAttr
                  (True, False) -> withAttr highlightedElementAttr
                  _             -> id

          number = 
            case (submitted, i+1 == k) of
              (False, _)    -> str (show (i+1) <> ". ")
              (True, False) -> withAttr incorrectElementAttr (str (show k <> ". "))
              (True, True ) -> withAttr correctElementAttr (str (show k <> ". "))
      in
        number <+> color (drawDescr text)

  _ -> error "cardstate mismatch"

----------------------------------------------------
---------------------- Events ----------------------
----------------------------------------------------

handleEvent :: GlobalState -> CS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s (VtyEvent e) =
  let update = updateCS gs
      continue' = continue . update
      halt' = flip goToModeOrQuit CardSelector in
    case e of
      V.EvKey V.KEsc []                -> halt' gs
      V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt' gs
      V.EvKey V.KRight [V.MCtrl]       -> next gs s
      V.EvKey V.KLeft  [V.MCtrl]       -> previous gs s

      ev -> case (s ^. cardState, s ^. currentCard) of
        (DefinitionState{_flipped = f}, _) ->
          case ev of
            V.EvKey V.KEnter [] -> 
              if f
                then next gs s 
                else continue' $ s & cardState.flipped %~ not
            _ -> continue' s

        (MultipleChoiceState {_highlighted = i, _number = n, _tried = kvs}, MultipleChoice _ (CorrectOption j _) _) ->
          case ev of
            V.EvKey V.KUp [] -> continue' up
            V.EvKey (V.KChar 'k') [] -> continue' up
            V.EvKey V.KDown [] -> continue' down 
            V.EvKey (V.KChar 'j') [] -> continue' down

            V.EvKey V.KEnter [] ->
                if frozen
                  then next gs s
                  else continue' $ s & cardState.tried %~ M.insert i True

            _ -> continue' s

          where frozen = M.findWithDefault False j kvs
            
                down = if i < n-1 && not frozen
                        then s & (cardState.highlighted) +~ 1
                        else s

                up = if i > 0 && not frozen
                      then s & (cardState.highlighted) -~ 1
                      else s
        
        (MultipleAnswerState {_highlighted = i, _number = n, _entered = submitted}, MultipleAnswer {}) ->
          case ev of
            V.EvKey V.KUp [] -> continue' up
            V.EvKey (V.KChar 'k') [] -> continue' up
            V.EvKey V.KDown [] -> continue' down 
            V.EvKey (V.KChar 'j') [] -> continue' down

            V.EvKey (V.KChar 'c') [] -> continue' $ s & (cardState.entered) .~ True

            V.EvKey V.KEnter [] ->
                if frozen
                  then next gs s
                  else continue' $ s & cardState.selected %~ M.adjust not i

            _ -> continue' s


          where frozen = submitted
            
                down = if i < n-1 && not frozen
                        then s & (cardState.highlighted) +~ 1
                        else s

                up = if i > 0 && not frozen
                      then s & (cardState.highlighted) -~ 1
                      else s

        (OpenQuestionState {_highlighted = i, _number = n, _gapInput = kvs, _correctGaps = cGaps}, OpenQuestion _ perforated) ->
          let correct = M.foldr (&&) True cGaps in
            case ev of
              V.EvKey (V.KFun 1) [] -> continue' $
                s & cardState.gapInput .~ correctAnswers
                  & cardState.entered .~ True
                  & cardState.correctGaps .~ M.fromAscList [(i, True) | i <- [0..n-1]]
                      where correctAnswers = M.fromAscList $ zip [0..] $ map NE.head (sentenceToGaps (perforatedToSentence perforated))

              V.EvKey (V.KChar '\t') [] -> continue' $ 
                if i < n - 1 && not correct
                  then s & (cardState.highlighted) +~ 1
                  else s & (cardState.highlighted) .~ 0
              
              V.EvKey V.KRight [] -> continue' $ 
                if i < n - 1 && not correct
                  then s & (cardState.highlighted) +~ 1
                  else s

              V.EvKey V.KLeft [] -> continue' $ 
                if i > 0 && not correct
                  then s & (cardState.highlighted) -~ 1
                  else s

              V.EvKey (V.KChar c) [] -> continue' $
                if correct then s else s & cardState.gapInput.at i.non "" %~ (++[c])

              V.EvKey V.KEnter [] -> if correct then next gs s else continue' s'
                  where sentence = perforatedToSentence perforated
                        gaps = sentenceToGaps sentence

                        s' = s & (cardState.correctGaps) %~ M.mapWithKey (\j _ -> M.findWithDefault "" j kvs `elem` gaps !! j)  & (cardState.entered) .~ True

              V.EvKey V.KBS [] -> continue' $ 
                  if correct then s else s & cardState.gapInput.ix i %~ backspace
                where backspace "" = ""
                      backspace xs = init xs
              _ -> continue' s
          
        (ReorderState {_highlighted = i, _entered = submitted, _grabbed=dragging, _number = n }, Reorder _ _) ->
          case ev of
            V.EvKey V.KUp [] -> continue' up
            V.EvKey (V.KChar 'k') [] -> continue' up
            V.EvKey V.KDown [] -> continue' down 
            V.EvKey (V.KChar 'j') [] -> continue' down

            V.EvKey (V.KChar 'c') [] -> continue' $ s & (cardState.entered) .~ True

            V.EvKey V.KEnter [] ->
                if frozen
                  then next gs s
                  else continue' $ s & cardState.grabbed %~ not

            _ -> continue' s


          where frozen = submitted
            
                down = 
                  case (frozen, i < n - 1, dragging) of
                    (True, _, _)  -> s
                    (_, False, _) -> s
                    (_, _, False) -> s & (cardState.highlighted) +~ 1
                    (_, _, True)  -> s & (cardState.highlighted) +~ 1
                                      & (cardState.order) %~ interchange i (i+1)

                up =
                  case (frozen, i > 0, dragging) of
                    (True, _, _)  -> s
                    (_, False, _) -> s
                    (_, _, False) -> s & (cardState.highlighted) -~ 1
                    (_, _, True)  -> s & (cardState.highlighted) -~ 1
                                      & (cardState.order) %~ interchange i (i-1)

        _ -> error "impossible"
handleEvent gs _ _ = continue gs

next :: GlobalState -> CS -> EventM Name (Next GlobalState)
next gs s
  | s ^. index + 1 < length (s ^. cards) = continue . updateCS gs . straightenState $ s & index +~ 1
  | otherwise                            = continue $ popState  gs

previous :: GlobalState -> CS -> EventM Name (Next GlobalState)
previous gs s | s ^. index > 0 = continue . updateCS gs . straightenState $ s & index -~ 1
              | otherwise      = continue gs

straightenState :: CS -> CS
straightenState s =
  let card = (s ^. cards) !! (s ^. index) in s
    & currentCard .~ card
    & cardState .~ defaultCardState card
  
interchange :: (Ord a) => a -> a -> Map a b -> Map a b
interchange i j kvs =
  let vali = kvs M.! i
      valj = kvs M.! j in
  M.insert j vali (M.insert i valj kvs)