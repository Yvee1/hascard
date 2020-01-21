{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Data.List.Split
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Lens.Micro.Platform

--                     Word   Description
data Card = Definition String String
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

drawCardUI :: State -> Widget Name
drawCardUI s = C.center $
               B.border $
               hLimitPercent 60 $
               vLimitPercent 40 $
               padTopBottom 1 $
               padLeftRight 5 $
               C.hCenter (str title) <=> str descr
  where Definition title descr = (s ^. cards) !! (s ^. index)

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s (VtyEvent (V.EvKey V.KEnter []))      = continue $ next s
handleEvent s _                                     = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

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
stringToCard s = let (firstLine : rest) = dropWhile (`elem` ["\n", "\r\n", "\r", ""]) (lines s) in
  case firstLine of
    ('#' : xs) -> Definition (dropWhile isSpace xs) (concat rest)
    _ -> error ("encountered an invalid card: \n" ++ show (lines s))

splitString :: String -> [String]
splitString = splitOn "---"

next :: State -> State
next s = s & index %~ (+1)
