{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Data.List.Split
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char
import Data.List (isPrefixOf)
import Brick
import Lens.Micro.Platform
import qualified Data.List.NonEmpty as NE
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

data Type = Incorrect | Correct
  deriving Show
data CorrectOption = CorrectOption Int String
  deriving Show
newtype IncorrectOption = IncorrectOption String
  deriving Show
data Answer = Answer Type String
  deriving Show

--                     Word   Description
data Card = Definition String String
          | MultipleChoice String CorrectOption [IncorrectOption]
          | MultipleAnswer String (NE.NonEmpty Answer) 
  deriving Show

data State = State
  { _cards    :: [Card]   -- list of flashcards
  , _index    :: Int      -- current card index
  , _correct  :: Int      -- not implemented, but for score keeping
  }

makeLenses ''State

type Event = ()
type Name = ()

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

listMultipleChoice :: CorrectOption -> [IncorrectOption] -> String
listMultipleChoice c = unlines . reverse . listMultipleChoice' [] 0 c
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
                              drawHeader title <=> drawDescr descr
                              
    MultipleChoice question correct others -> drawCardBox $
                                              drawHeader question <=> drawDescr (listMultipleChoice correct others)

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
handleEvent s (VtyEvent (V.EvKey V.KEnter []))              = next s
handleEvent s _                                             = continue s

titleAttr :: AttrName
titleAttr = attrName "title"

textboxAttr :: AttrName
textboxAttr = attrName "textbox"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [(titleAttr, bg V.green `V.withStyle` V.bold `V.withStyle` V.underline)
  ,(textboxAttr, V.defAttr)
  ]
 
handleFilePath :: FilePath -> IO String
handleFilePath = readFile

runBrickFlashcards :: String -> IO ()
runBrickFlashcards input = do
  let cards = stringToCards input
  let initialState = State cards 0 0
  finalState <- defaultMain app initialState
  pure ()

stringToCards :: String -> [Card]
stringToCards = map stringToCard . splitString

stringToCard :: String -> Card
stringToCard s = let (fstLine : ls@(sndLine : rest)) = dropWhile (`elem` ["\n", "\r\n", "\r", ""]) (lines s) in
  case (fstLine, sndLine) of
    ('#' : ' ' : question, '-' : ' ' : _) -> makeMultipleChoice question ls
    ('#' : ' ' : question, '*' : ' ' : _) -> makeMultipleChoice question ls
    ('#' : ' ' : title, _)           -> makeDefinition title ls
    _                           -> error ("encountered an invalid card: \n" ++ show (lines s))

makeDefinition :: String -> [String] -> Card
makeDefinition title descr = Definition title (concat descr)

makeMultipleChoice :: String -> [String] -> Card
makeMultipleChoice question ls = MultipleChoice question correct incorrects
  where (correct, incorrects) = makeMultipleChoice' [] [] 0 ls
        makeMultipleChoice' [] _ _ [] = error ("multiple choice had no correct answer: \n" ++ unlines (question : ls))
        makeMultipleChoice' [c] ics _ [] = (c, reverse ics)
        makeMultipleChoice' _ _ _ [] = error ("multiple choice had multiple correct answers: \n" ++ unlines (question : ls))
        makeMultipleChoice' cs ics i (('-' : ' ' : opt) : opts) = makeMultipleChoice' cs (IncorrectOption opt : ics) (i+1) opts
        makeMultipleChoice' cs ics i (('*' : ' ' : opt) : opts) = makeMultipleChoice' (CorrectOption i opt : cs) ics (i+1) opts

splitString :: String -> [String]
splitString = splitOn "---"

next :: State -> EventM Name (Next State)
next s = if (s ^. index + 1) < length (s ^. cards)
          then continue $ s & index %~ (+1)
          else halt s
