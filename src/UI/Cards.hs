module UI.Cards (Card, State(..), drawUI, handleEvent, theMap) where

import Brick
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Types
import States
import StateManagement
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe
import Text.Wrap
import Data.Text (pack)
import UI.Attributes
import UI.BrickHelpers
import System.FilePath
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
drawUI s =  [maybe emptyWidget (`drawPopup` s) (s^.popup), drawCardUI s <=> drawInfo s]

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

drawFooter :: CS -> Widget Name
drawFooter s = if s^.reviewMode 
  then padLeftRight 1 $ wrong <+> progress <+> correct
  else progress
  -- not guaranteed that progress is horizontally centered i think
  where progress = C.hCenter $ str (show (s^.index + 1) ++ "/" ++ show (s^.nCards))
        wrong = withAttr wrongAttr (str ("✗ " <> show nWrong)) 
        correct = withAttr correctAttr (str ("✓ " <> show nCorrect))
        nCorrect = length (s^.correctCards)
        nWrong = s^.index - nCorrect + (if endCard then 1 else 0)
        endCard = maybe False (isFinalPopup . view popupState) (s^.popup)

drawCardUI :: CS -> Widget Name
drawCardUI s = let p = 1 in
  joinBorders $ drawCardBox $ (<=> drawFooter s) $
  case (s ^. cards) !! (s ^. index) of
    Definition title descr -> drawHeader title
                          <=> B.hBorder
                          <=> padLeftRight p (drawDef s descr <=> str " ")
                              
    MultipleChoice question correct others -> drawHeader question 
                                          <=> B.hBorder 
                                          <=> padLeftRight p (drawChoices s (listMultipleChoice correct others) <=> str " ")

    OpenQuestion title perforated -> drawHeader title
                                 <=> B.hBorder
                                 <=> padLeftRight p (atLeastV 1 (drawPerforated s perforated) <=> str " ")

    MultipleAnswer question options -> drawHeader question
                                   <=> B.hBorder
                                   <=> padRight (Pad p) (drawOptions s options <=> str " ")

    Reorder question _ -> drawHeader question
                      <=> B.hBorder
                      <=> padLeftRight p (drawReorder s <=> str " ")

drawHeader :: String -> Widget Name
drawHeader title = withAttr titleAttr $
                   padLeftRight 1 $
                   hCenteredStrWrap title

wrapSettings :: WrapSettings
wrapSettings = WrapSettings {preserveIndentation=False, breakLongWords=True}

drawDescr :: String -> Widget Name
drawDescr = strWrapWith wrapSettings

drawDef :: CS -> String -> Widget Name
drawDef s def = if s ^. showHints then drawHintedDef s def else drawNormalDef s def

drawHintedDef :: CS -> String -> Widget Name
drawHintedDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f then drawDescr def else drawDescr [if isSpace' char then char else '_' | char <- def]
  _ -> error "impossible: " 

isSpace' :: Char -> Bool
isSpace' '\r' = True
isSpace' a    = isSpace a

drawNormalDef:: CS -> String -> Widget Name
drawNormalDef s def = case s ^. cardState of
  DefinitionState {_flipped=f} -> if f
    then drawDescr def
    else Widget Greedy Fixed $ do
      c <- getContext
      let w = c^.availWidthL
      render . vBox $ [str " " | _ <- wrapTextToLines wrapSettings w (pack def)]
  _ -> error "impossible: " 

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
            cursor = if i == j then showCursor Ordinary (Location (textWidth gap, 0)) else id

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

drawReorder :: CS -> Widget Name
drawReorder s = case (s ^. cardState, s ^. currentCard) of
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
halt' :: GlobalState -> EventM n (Next GlobalState)
halt' = flip (removeToModeOrQuit' (\(CardSelectorState s) -> CardSelectorState <$> refreshRecents s)) CardSelector

handleEvent :: GlobalState -> CS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s (VtyEvent e) =
  let update = updateCS gs
      continue' = continue . update in
    case e of
      V.EvKey V.KEsc []                -> popStateOrQuit gs
      V.EvKey V.KRight [V.MCtrl]       -> if not (s^.reviewMode) then next gs s else continue gs
      V.EvKey V.KLeft  [V.MCtrl]       -> if not (s^.reviewMode) then previous gs s else continue gs

      ev ->
        flip (`maybe` (\p -> handlePopupEvent p gs s ev)) (s ^. popup) $
          case (s ^. cardState, s ^. currentCard) of
            (DefinitionState{_flipped = f}, _) ->
              case ev of
                V.EvKey V.KEnter [] -> 
                  if f
                    then if not (s^.reviewMode) then next gs s 
                      else continue' (s & popup ?~ correctPopup)
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
                      then next gs $ s & if correctlyAnswered then correctCards %~ (s^.index:) else id
                      else continue' $ s & cardState.tried %~ M.insert i True
                                        

                _ -> continue' s

              where frozen = M.findWithDefault False j kvs
                
                    down = if i < n-1 && not frozen
                            then s & (cardState.highlighted) +~ 1
                            else s

                    up = if i > 0 && not frozen
                          then s & (cardState.highlighted) -~ 1
                          else s

                    correctlyAnswered = i == j && M.size (M.filter (==True) kvs) == 1
            
            (MultipleAnswerState {_highlighted = i, _number = n, _entered = submitted, _selected = kvs}, MultipleAnswer _ opts) ->
              case ev of
                V.EvKey V.KUp [] -> continue' up
                V.EvKey (V.KChar 'k') [] -> continue' up
                V.EvKey V.KDown [] -> continue' down 
                V.EvKey (V.KChar 'j') [] -> continue' down

                V.EvKey (V.KChar 'c') [] -> continue' $ s & (cardState.entered) .~ True

                V.EvKey V.KEnter [] ->
                    if frozen
                      then next gs $ s & if correctlyAnswered then correctCards %~ (s^.index:) else id
                      else continue' $ s & cardState.selected %~ M.adjust not i

                _ -> continue' s


              where frozen = submitted
                
                    down = if i < n-1 && not frozen
                            then s & (cardState.highlighted) +~ 1
                            else s

                    up = if i > 0 && not frozen
                          then s & (cardState.highlighted) -~ 1
                          else s

                    correctlyAnswered = NE.toList (NE.map isOptionCorrect opts) == map snd (M.toAscList kvs)

            (OpenQuestionState {_highlighted = i, _number = n, _gapInput = kvs, _correctGaps = cGaps, _failed=fail}, OpenQuestion _ perforated) ->
              let frozen = M.foldr (&&) True cGaps in
                case ev of
                  V.EvKey (V.KFun 1) [] -> continue' $
                    s & cardState.gapInput .~ correctAnswers
                      & cardState.entered .~ True
                      & cardState.failed .~ True
                      & cardState.correctGaps .~ M.fromAscList [(i, True) | i <- [0..n-1]]
                          where correctAnswers = M.fromAscList $ zip [0..] $ map NE.head (sentenceToGaps (perforatedToSentence perforated))

                  V.EvKey (V.KChar '\t') [] -> continue' $ 
                    if i < n - 1 && not frozen
                      then s & (cardState.highlighted) +~ 1
                      else s & (cardState.highlighted) .~ 0
                  
                  V.EvKey V.KRight [] -> continue' $ 
                    if i < n - 1 && not frozen
                      then s & (cardState.highlighted) +~ 1
                      else s

                  V.EvKey V.KLeft [] -> continue' $ 
                    if i > 0 && not frozen
                      then s & (cardState.highlighted) -~ 1
                      else s

                  V.EvKey (V.KChar c) [] -> continue' $
                    if frozen then s else s & cardState.gapInput.at i.non "" %~ (++[c])

                  V.EvKey V.KEnter [] -> if frozen
                    then if fail
                      then next gs s
                      else next gs (s & correctCards %~ (s^.index:)) 
                    else continue' s'
                      where sentence = perforatedToSentence perforated
                            gaps = sentenceToGaps sentence

                            s' = s & (cardState.correctGaps) %~ M.mapWithKey (\j _ -> M.findWithDefault "" j kvs `elem` gaps !! j)
                                  & (cardState.entered) .~ True

                            s'' = if M.foldr (&&) True (s' ^. cardState.correctGaps)
                                    then s'
                                    else s' & cardState.failed .~ True

                  V.EvKey V.KBS [] -> continue' $ 
                      if frozen then s else s & cardState.gapInput.ix i %~ backspace
                    where backspace "" = ""
                          backspace xs = init xs
                  _ -> continue' s
              
            (ReorderState {_highlighted = i, _entered = submitted, _grabbed=dragging, _number = n, _order = kvs }, Reorder _ elts) ->
              case ev of
                V.EvKey V.KUp [] -> continue' up
                V.EvKey (V.KChar 'k') [] -> continue' up
                V.EvKey V.KDown [] -> continue' down 
                V.EvKey (V.KChar 'j') [] -> continue' down

                V.EvKey (V.KChar 'c') [] -> continue' $ s & (cardState.entered) .~ True

                V.EvKey V.KEnter [] ->
                    if frozen
                      then next gs $ s & if correct then correctCards %~ (s^.index:) else id
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
                                        
                    correct = all (uncurry (==) . (\i -> (i+1, fst (kvs M.! i)))) [0..n-1]

            _ -> error "impossible"
handleEvent gs _ _ = continue gs

next :: GlobalState -> CS -> EventM Name (Next GlobalState)
next gs s
  | s ^. index + 1 < length (s ^. cards) = continue . updateCS gs . straightenState $ s & index +~ 1
  | s ^. reviewMode                      = 
      let thePopup = 
            if null (s^.correctCards) || length (s^. correctCards) == length (s^.cards)
              then finalPopup
              else deckMakerPopup
      in continue . updateCS gs $ s & popup ?~ thePopup
  | otherwise                            = halt' gs

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

----------------------------------------------------
---------------------- Popups ----------------------
----------------------------------------------------

isFinalPopup :: PopupState -> Bool
isFinalPopup FinalPopup       = True
isFinalPopup DeckMakerPopup{} = True
isFinalPopup _                = False

correctPopup :: Popup CS
correctPopup = Popup drawer eventHandler initialState
  where drawer s =
          let selected = maybe 0 (^?! popupState.popupSelected) (s^.popup)
              colorNo  = if selected == 0 then selectedNoButtonAttr else noButtonAttr
              colorYes = if selected == 1 then selectedYesButtonAttr else yesButtonAttr
              no = withAttr colorNo $ str "No"
              yes = withAttr colorYes $ str "Yes" in
                centerPopup $ 
                B.borderWithLabel (str "Correct?") $
                hLimit 20 $
                str " " <=>
                str " " <=>
                (hFill ' ' <+> no <+> hFill ' ' <+> yes <+> hFill ' ')

        initialState = CorrectPopup 0

        eventHandler gs s ev = 
          let update = updateCS gs
              continue' = continue . update
              p = fromJust (s ^. popup)
            in case ev of
              V.EvKey V.KLeft  [] -> continue' $ s & popup ?~ (p & popupState.popupSelected .~ 0)
              V.EvKey V.KRight [] -> continue' $ s & popup ?~ (p & popupState.popupSelected .~ 1)
              -- V.EvKey V.KRight [] -> s & popup .~ popupState.popupSelected .~ Just 1
              V.EvKey V.KEnter [] -> next gs $ s & popup .~ Nothing
                                                 & if p ^?! popupState.popupSelected == 1 then correctCards %~ (s^.index:) else id
              _ -> continue' s

finalPopup :: Popup CS
finalPopup = Popup drawer eventHandler initialState
  where drawer s = 
          let wrong    = withAttr wrongAttr   (str (" Incorrect: " <> show nWrong)   <+> hFill ' ') 
              correct  = withAttr correctAttr (str (" Correct:   " <> show nCorrect) <+> hFill ' ')
              nCorrect = length (s^.correctCards)
              nWrong   = s^.index + 1 - nCorrect in
                centerPopup $ 
                B.borderWithLabel (str "Finished") $
                hLimit 20 $
                str " " <=>
                wrong <=>
                correct

        initialState = FinalPopup

        eventHandler gs s (V.EvKey V.KEnter []) = halt' gs
        eventHandler gs _ _ = continue gs

deckMakerPopup :: Popup CS
deckMakerPopup = Popup drawer eventHandler initialState
  where drawer s =
          let state    = fromMaybe initialState $ view popupState <$> s^.popup
              j = state ^?! popupSelected

              makeSym lens i = case (state ^?! lens, i == j) of
                (_, True) -> withAttr highlightedOptAttr $ str "*"
                (True, _) -> withAttr selectedOptAttr    $ str "*"
                _         -> withAttr selectedOptAttr    $ str " "
            
              makeBox lens i = 
                (if state ^?! lens then withAttr selectedOptAttr else id) $
                  str "[" <+> makeSym lens i <+> str "]"

              wBox = makeBox makeDeckIncorrect 0
              cBox = makeBox makeDeckCorrect 1

              wrong    = wBox <+> withAttr wrongAttr   (str (" Incorrect: " <> show nWrong)   <+> hFill ' ') 
              correct  = cBox <+> withAttr correctAttr (str (" Correct:   " <> show nCorrect) <+> hFill ' ')
              nCorrect = length (s^.correctCards)
              nWrong   = s^.index + 1 - nCorrect in
                centerPopup $ 
                B.borderWithLabel (str "Generate decks") $
                hLimit 20 $
                str " " <=>
                wrong <=>
                correct <=>
                str " " <=>
                C.hCenter ((if j == 2 then withAttr selectedAttr else id) (str "Ok"))

        initialState = DeckMakerPopup 0 False False

        eventHandler gs s ev =
          let update = updateCS gs
              continue' = continue . update
              p = fromJust (s ^. popup)
              state = p ^. popupState
          in case state ^?! popupSelected of
            0 -> case ev of
              V.EvKey V.KEnter []      -> continue' $ s & popup ?~ (p & popupState.makeDeckIncorrect %~ not)
              V.EvKey V.KDown  []      -> continue' $ s & popup ?~ (p & popupState.popupSelected +~ 1)
              V.EvKey (V.KChar 'j') [] -> continue' $ s & popup ?~ (p & popupState.popupSelected +~ 1)
              _ -> continue' s
            1 -> case ev of
              V.EvKey V.KEnter []      -> continue' $ s & popup ?~ (p & popupState.makeDeckCorrect %~ not)
              V.EvKey V.KDown  []      -> continue' $ s & popup ?~ (p & popupState.popupSelected +~ 1)
              V.EvKey (V.KChar 'j') [] -> continue' $ s & popup ?~ (p & popupState.popupSelected +~ 1)
              V.EvKey V.KUp  []        -> continue' $ s & popup ?~ (p & popupState.popupSelected -~ 1)
              V.EvKey (V.KChar 'k') [] -> continue' $ s & popup ?~ (p & popupState.popupSelected -~ 1)
              _ -> continue' s
            2 -> case ev of
              V.EvKey V.KEnter []      -> liftIO (generateDecks (s ^. pathToFile) (s ^. cards) (s ^. correctCards) (state ^?! makeDeckCorrect) (state ^?! makeDeckIncorrect))
                                       *> halt' gs
              V.EvKey V.KUp  []        -> continue' $ s & popup ?~ (p & popupState.popupSelected -~ 1)
              V.EvKey (V.KChar 'k') [] -> continue' $ s & popup ?~ (p & popupState.popupSelected -~ 1)
              _ -> continue' s

generateDecks :: FilePath -> [Card] -> [Int] -> Bool -> Bool -> IO ()
generateDecks fp cards corrects makeCorrect makeIncorrect = 
  when (makeCorrect || makeIncorrect) $ 
    do let (correct, incorrect) = splitCorrectIncorrect cards corrects
       when makeCorrect   $ writeFile (replaceBaseName fp (takeBaseName fp <> "+")) (cardsToString correct)
       when makeIncorrect $ writeFile (replaceBaseName fp (takeBaseName fp <> "-")) (cardsToString incorrect)

-- gets list of cards, list of indices of correct cards; returns (correct, incorrect)
splitCorrectIncorrect :: [Card] -> [Int] -> ([Card], [Card])
splitCorrectIncorrect cards indices = doSplit [] [] (zip [0..] cards) (reverse indices)
  where doSplit cs ws [] _  = (reverse cs, reverse ws)
        doSplit cs ws ((_, x):xs) [] = doSplit cs (x:ws) xs []
        doSplit cs ws ((j, x):xs) (i:is) = 
          if i == j
            then doSplit (x:cs) ws xs is
            else doSplit cs (x:ws) xs (i:is)