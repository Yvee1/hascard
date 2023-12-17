{-# LANGUAGE FlexibleContexts #-}
module UI.Cards (Card, State(..), drawUI, handleEvent, theMap) where

import Brick
import Control.Monad
import Control.Monad.Extra (whenM, notM, unlessM)
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Lens.Micro.Platform
import Types
import States
import StateManagement
import Data.Char (isSpace, toLower)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.List.Split
import Debug
import Text.Wrap
import Data.Text (pack)
import UI.Attributes
import UI.BrickHelpers
import System.FilePath
import Data.List (intercalate)
import qualified Brick.Types as BT
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
    OpenQuestionState {}   -> ", LEFT/RIGHT/TAB: navigate gaps, ENTER: submit answer / continue, Ctrl+F1: show answer"
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
drawCardUI s = 
  let card = (s ^. shownCards) !! (s ^. index)
  in
  joinBorders $ 
  drawCardBox $ 
  drawHeader card
  <=>
  B.hBorder
  <=>
  scrollableViewportPercent 60 (CardViewport (s ^. index))
  (drawContent s card)
  <=>
  str " "
  <=>
  drawFooter s

drawHeader :: Card -> Widget Name
drawHeader (Definition title _ _) = drawTitle title
drawHeader (MultipleChoice question _ _ _) = drawTitle question
drawHeader (OpenQuestion question _ _) = drawTitle question
drawHeader (MultipleAnswer question _ _) = drawTitle question
drawHeader (Reorder question _ _) = drawTitle question

drawContent :: CS -> Card -> Widget Name
drawContent s (Definition _ _ descr) = padLeftRight 1 $ drawDef s descr
drawContent s (MultipleChoice _ _ correct others) = padLeftRight 1 $ drawChoices s (listMultipleChoice correct others)
drawContent s (OpenQuestion _ _ perforated) = padLeftRight 1 $ drawPerforated s perforated
drawContent s (MultipleAnswer _ _ options) = padRight (Pad 1) $ drawOptions s options
drawContent s (Reorder{}) = padLeftRight 1 $ drawReorder s

drawTitle :: String -> Widget n
drawTitle title = withAttr titleAttr $
                   padLeftRight 1 $
                   hCenteredStrWrap title

wrapSettings :: WrapSettings
wrapSettings = defaultWrapSettings {preserveIndentation=False, breakLongWords=True}

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
  (MultipleChoiceState {_highlighted=i, _tried=kvs}, MultipleChoice _ _ (CorrectOption k _) _)  -> vBox formattedOptions

             where formattedOptions :: [Widget Name]
                   formattedOptions = [ visibility $ prefix <+> coloring (drawDescr opt) |
                                        (j, opt) <- zip [0..] options,
                                        let prefix = if i == j then withAttr highlightedChoiceAttr (str "* ") else str "  "
                                            chosen = M.findWithDefault False j kvs
                                            visibility = if i == j && not chosen then visible else id
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
      where drawOption (Option kind text, i) = visibility $ coloring (str "[") <+> coloring (highlighting (str symbol)) <+> coloring (str "] ") <+> drawDescr text
              where symbol = if (i == j && not submitted) || enabled then "*" else " "
                    enabled = M.findWithDefault False i kvs
                    highlighting = if i == j && not submitted then withAttr highlightedOptAttr else id
                    visibility = if i == j && not submitted then visible else id
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
            stored = M.findWithDefault "" i kvs
            gap = if stored == "" then "░" else stored
            n' =  w - n - textWidth gap

            cursor :: Widget Name -> Widget Name
            -- i is the index of the gap that we are drawing; j is the gap that is currently selected
            cursor = if i == j then showCursor Ordinary (Location (textWidth gap, 0)) else id

            visibility = if i == j && not submitted then visible else id
            correct = M.findWithDefault False i cgs
            coloring = case (submitted, correct) of
              (False, _) -> withAttr gapAttr
              (True, False) -> withAttr incorrectGapAttr
              (True, True) -> withAttr correctGapAttr

            gapWidget = visibility . cursor $ coloring (str gap) in

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
        prefix = if head s `elem` ['\n', '\r'] then T.pack " " else T.empty
        postfix = if lastLetter == ' ' then T.pack [lastLetter] else T.empty
        ts = wrapTextToLines wrapSettings w (pack s') & ix 0 %~ (if startsWithSpace then (T.pack " " `T.append`) . T.drop (padding + 1) else T.drop padding)
        ts' = prefix : (ts & _last %~ (`T.append` postfix))
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
  (ReorderState {_highlighted=j, _grabbed=g, _order=kvs, _number=n, _entered=submitted}, Reorder{}) ->
    vBox . flip map (map (\i -> (i, kvs M.! i)) [0..n-1]) $
    \(i, (k, text)) ->
      let color = case (i == j,  g) of
                  (True, True ) -> withAttr grabbedElementAttr
                  (True, False) -> withAttr highlightedElementAttr
                  _             -> id

          visibility = if i == j && not submitted then visible else id

          number =
            case (submitted, i+1 == k) of
              (False, _)    -> str (show (i+1) <> ". ")
              (True, False) -> withAttr incorrectElementAttr (str (show k <> ". "))
              (True, True ) -> withAttr correctElementAttr (str (show k <> ". "))
      in visibility $ number <+> color (drawDescr text)

  _ -> error "cardstate mismatch"

----------------------------------------------------
---------------------- Events ----------------------
----------------------------------------------------
halt' :: EventM n GlobalState ()
halt' = removeToModeOrQuit' beforeMoving CardSelector
  where beforeMoving = zoom css refreshRecents

scroll :: CS -> Int -> EventM Name s ()
scroll s = scroll' $ s ^. index

scroll' :: Int -> Int -> EventM Name s ()
scroll' i = vScrollBy $ viewportScroll $ CardViewport i

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent (VtyEvent e) =
  -- let update = updateCS gs
  --     continue' = continue . update in
  case e of
    V.EvKey V.KEsc []          -> popStateOrQuit
    V.EvKey V.KRight [V.MCtrl] -> (whenM.notM.use $ cs.reviewMode) next
    V.EvKey V.KLeft  [V.MCtrl] -> (whenM.notM.use $ cs.reviewMode) previous

    ev -> do
      pUp <- use $ cs.popup
      s <- use cs
      flip (`maybe` (`handlePopupEvent` ev)) pUp $
        case (s ^. cardState, s ^. currentCard) of
          (DefinitionState{_flipped = f}, Definition {definition = d}) ->
            case ev of
              V.EvKey V.KEnter []  ->
                if f || all isSpace d 
                  then if not (s^.reviewMode) then next
                    else cs.popup ?= correctPopup
                  else cs.cardState.flipped %= not
              V.EvKey V.KUp [] -> up
              V.EvKey (V.KChar 'k') [] -> up
              V.EvKey V.KDown [] -> down
              V.EvKey (V.KChar 'j') [] -> down
              _ -> return ()

              where up = when f $ scroll s (-1)
                    down = when f $ scroll s 1

          (MultipleChoiceState {_highlighted = i, _number = n, _tried = kvs}, MultipleChoice _ _ (CorrectOption j _) _) ->
            case ev of
              V.EvKey V.KUp [] -> up
              V.EvKey (V.KChar 'k') [] -> up
              V.EvKey V.KDown [] -> down
              V.EvKey (V.KChar 'j') [] -> down

              V.EvKey V.KEnter [] ->
                  if frozen
                    then do when correctlyAnswered $ cs.correctCards %= (s^.index:)
                            next
                    else cs.cardState.tried %= M.insert i True
              _ -> return ()

            where frozen = M.findWithDefault False j kvs

                  down = if not frozen 
                         then when (i < n-1) $ cs.cardState.highlighted += 1
                         else scroll s 1

                  up = if not frozen 
                       then when (i > 0) $ cs.cardState.highlighted -= 1
                       else scroll s (-1)

                  correctlyAnswered = i == j && M.size (M.filter id kvs) == 1

          (MultipleAnswerState {_highlighted = i, _number = n, _entered = submitted, _selected = kvs}, MultipleAnswer _ _ opts) ->
            case ev of
              V.EvKey V.KUp [] -> up
              V.EvKey (V.KChar 'k') [] -> up
              V.EvKey V.KDown [] -> down
              V.EvKey (V.KChar 'j') [] -> down

              V.EvKey (V.KChar 'c') [] -> cs.cardState.entered .= True

              V.EvKey V.KEnter [] ->
                  if frozen
                    then do when correctlyAnswered $ cs.correctCards %= (s^.index:)
                            next
                    else cs.cardState.selected %= M.adjust not i
              V.EvKey (V.KChar '\t') [] ->
                  if frozen
                    then do when correctlyAnswered $ cs.correctCards %= (s^.index:)
                            next
                    else cs.cardState.selected %= M.adjust not i


              _ -> return ()


            where frozen = submitted

                  down = if not frozen 
                         then when (i < n-1) $ cs.cardState.highlighted += 1
                         else scroll s 1

                  up = if not frozen 
                       then when (i > 0) $ cs.cardState.highlighted -= 1
                       else scroll s (-1)

                  correctlyAnswered = NE.toList (NE.map isOptionCorrect opts) == map snd (M.toAscList kvs)

          (OpenQuestionState {_highlighted = i, _number = n, _gapInput = kvs, _correctGaps = cGaps, _failed=fail}, OpenQuestion _ _ perforated) ->
            case ev of
              V.EvKey (V.KFun 1) [V.MCtrl] -> zoom (cs.cardState) $ do
                gapInput .= correctAnswers
                entered .= True
                failed .= True
                correctGaps .= M.fromAscList [(i, True) | i <- [0..n-1]]
                      where correctAnswers = M.fromAscList $ zip [0..] $ map NE.head (sentenceToGaps (perforatedToSentence perforated))

              V.EvKey (V.KChar '\t') [] -> zoom (cs.cardState) $ do
                if i < n - 1 && not frozen
                  then highlighted += 1
                  else highlighted .= 0

              V.EvKey V.KRight [] -> 
                when (i < n - 1 && not frozen) $
                  cs.cardState.highlighted += 1

              V.EvKey V.KLeft [] ->
                when (i > 0 && not frozen) $
                  cs.cardState.highlighted -= 1

              -- C-w deletes a word back (eg. "test test" -> "test")
              V.EvKey (V.KChar 'w') [V.MCtrl] -> zoom (cs.cardState) $ do
                  unless frozen $ gapInput.ix i %= backword
                where backword "" = ""
                      backword xs = unwords . init . words $ xs

              V.EvKey V.KUp [] -> up
              V.EvKey V.KDown [] -> down

              V.EvKey (V.KChar c) [] -> zoom (cs.cardState) $ do
                  unless frozen $ gapInput.at i.non "" %= (++[c])
                  case c of
                    'k' -> up
                    'j' -> down
                    _ -> return ()

              V.EvKey V.KEnter [] -> case (frozen, fail) of
                (False, _) -> zoom cs $ do
                  let sentence = perforatedToSentence perforated
                      gaps = sentenceToGaps sentence

                      wordIsCorrect :: String -> NonEmpty String -> Bool
                      wordIsCorrect = if s^.isCaseSensitive
                        then elem
                        else (\word possibilites -> map toLower word `elem` NE.map (map toLower) possibilites)

                  cardState.correctGaps %= M.mapWithKey (\j _ -> M.findWithDefault "" j kvs `wordIsCorrect` (gaps !! j))
                  cardState.entered .= True

                  unlessM (M.foldr (&&) True <$> use (cardState.correctGaps)) $
                    cardState.failed .= True

                (_, True) -> next
                (_, False) -> do
                  cs.correctCards %= (s^.index:)
                  next

              V.EvKey V.KBS [] -> unless frozen $
                  cs.cardState.gapInput.ix i %= backspace
                where backspace "" = ""
                      backspace xs = init xs

              _ -> return ()

              where frozen = M.foldr (&&) True cGaps
                    down = when frozen $ scroll s 1
                    up = when frozen $ scroll s (-1)

          (ReorderState {_highlighted = i, _entered = submitted, _grabbed=dragging, _number = n, _order = kvs }, Reorder _ _ elts) ->
            case ev of
              V.EvKey V.KUp [] -> up
              V.EvKey (V.KChar 'k') [] -> up
              V.EvKey V.KDown [] -> down
              V.EvKey (V.KChar 'j') [] -> down
              V.EvKey (V.KChar 'c') [] -> cs.cardState.entered .= True
              V.EvKey V.KEnter [] ->
                  if frozen
                    then do when correct $ cs.correctCards %= (s^.index:)
                            next
                    else cs.cardState.grabbed %= not

              _ -> return ()


            where frozen = submitted

                  down = zoom (cs.cardState) $
                    case (frozen, i < n - 1, dragging) of
                      (True, _, _)  -> scroll s 1
                      (_, False, _) -> return ()
                      (_, _, False) -> highlighted += 1
                      (_, _, True)  -> do highlighted += 1
                                          order %= interchange i (i+1)

                  up = zoom (cs.cardState) $
                    case (frozen, i > 0, dragging) of
                      (True, _, _)  -> scroll s (-1)
                      (_, False, _) -> return ()
                      (_, _, False) -> highlighted -= 1
                      (_, _, True)  -> do highlighted -= 1
                                          order %= interchange i (i-1)

                  correct = all (uncurry (==) . (\i -> (i+1, fst (kvs M.! i)))) [0..n-1]

          _ -> error "impossible"
handleEvent (BT.MouseDown (SBClick el (CardViewport i)) _ _ _) = handleClickScroll (scroll' i) el
handleEvent _ = return ()

next :: EventM Name GlobalState ()
next = do
  i <- use $ cs.index
  sc <- use $ cs.shownCards
  rm <- use $ cs.reviewMode
  case (i + 1 < length sc, rm) of
    (True, _) -> zoom cs $ do
      fp <- use pathToFile
      sc <- use shownCards
      liftIO (openCardExternal (takeDirectory fp) (sc !! (i + 1))) 
      index += 1
      straightenState
    (_, True) -> zoom cs $ do
      cc <- use correctCards
      let thePopup = 
            if null cc || length cc == length sc
              then finalPopup
              else deckMakerPopup
      popup ?= thePopup
    _ -> halt'

previous :: EventM Name GlobalState ()
previous = zoom cs $ do
  i <- use index
  when (i > 0) $ do
    fp <- use pathToFile
    sc <- use shownCards
    liftIO (openCardExternal (takeDirectory fp) (sc !! (i - 1))) 
    index -= 1
    straightenState

straightenState :: MonadState CS m => m ()
straightenState = do
  sc <- use shownCards
  i <- use index
  let card = sc !! i
  currentCard .= card
  cardState .= defaultCardState card

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

correctPopup :: Popup GlobalState CS
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

        eventHandler ev = do
          p <- fromJust <$> use (cs.popup)
          let ps = cs.popup._Just.popupState
          case ev of
            V.EvKey V.KLeft  [] -> ps.popupSelected .= 0
            V.EvKey V.KRight [] -> ps.popupSelected .= 1
            -- Adding vim shortcuts here
            V.EvKey (V.KChar 'h') [] -> ps.popupSelected .= 0
            V.EvKey (V.KChar 'l') [] -> ps.popupSelected .= 1

            V.EvKey V.KEnter [] -> do
               cs.popup .= Nothing
               when (p ^?! popupState.popupSelected == 1) $
                 do i <- use $ cs.index
                    cs.correctCards %= (i:) 
               next
            _ -> return ()

finalPopup :: Popup GlobalState CS
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

        eventHandler (V.EvKey V.KEnter []) = halt'
        eventHandler _ = return ()

deckMakerPopup :: Popup GlobalState CS
deckMakerPopup = Popup drawer eventHandler initialState
  where drawer s =
          let state    = maybe initialState (view popupState) (s^.popup)
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

        eventHandler ev = do
          im <- use $ cs.indexMapping
          ccs <- use $ cs.correctCards
          let originalCorrects = 
                sortOn negate (map (im !!) ccs)
          p <- fromJust <$> use (cs.popup)
          let ps = cs.popup._Just.popupState
          let state = p ^. popupState

          case state ^?! popupSelected of
            0 -> case ev of
              V.EvKey V.KEnter []      -> ps.makeDeckIncorrect %= not
              V.EvKey V.KDown  []      -> ps.popupSelected += 1
              V.EvKey (V.KChar 'j') [] -> ps.popupSelected += 1
              _ -> return ()
            1 -> case ev of
              V.EvKey V.KEnter []      -> ps.makeDeckCorrect %= not
              V.EvKey V.KDown  []      -> ps.popupSelected += 1
              V.EvKey (V.KChar 'j') [] -> ps.popupSelected += 1
              V.EvKey V.KUp  []        -> ps.popupSelected -= 1
              V.EvKey (V.KChar 'k') [] -> ps.popupSelected -= 1
              _ -> return ()
            2 -> case ev of
              V.EvKey V.KEnter []      -> do
                fp <- use $ cs.pathToFile
                ocs <- use $ cs.originalCards
                liftIO $ generateDecks fp ocs originalCorrects (state ^?! makeDeckCorrect) (state ^?! makeDeckIncorrect)
                halt'
              V.EvKey V.KUp  []        -> ps.popupSelected -= 1
              V.EvKey (V.KChar 'k') [] -> ps.popupSelected -= 1
              _ -> return ()

generateDecks :: FilePath -> [Card] -> [Int] -> Bool -> Bool -> IO ()
generateDecks fp cards corrects makeCorrect makeIncorrect =
  when (makeCorrect || makeIncorrect) $
    do let (correct, incorrect) = splitCorrectIncorrect cards corrects
       when makeCorrect   $ writeFile (replaceBaseName fp (takeBaseName fp <> "+")) (cardsToString correct)
       when makeIncorrect $ writeFile (replaceBaseName fp (takeBaseName fp <> "-")) (cardsToString incorrect)

-- gets list of cards, list of indices of correct cards in decreasing order; returns (correct, incorrect)
splitCorrectIncorrect :: [Card] -> [Int] -> ([Card], [Card])
splitCorrectIncorrect cards indices = doSplit [] [] (zip [0..] cards) (reverse indices)
  where doSplit cs ws [] _  = (reverse cs, reverse ws)
        doSplit cs ws ((_, x):xs) [] = doSplit cs (x:ws) xs []
        doSplit cs ws ((j, x):xs) (i:is) =
          if i == j
            then doSplit (x:cs) ws xs is
            else doSplit cs (x:ws) xs (i:is)
