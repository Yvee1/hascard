{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Data.List.Split
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char
import Brick
import Lens.Micro.Platform
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V


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

drawHeader :: String -> Widget Name
drawHeader title = padBottom (Pad 1) $
                   withAttr titleAttr $
                   C.hCenter (str title)

drawDescr :: String -> Widget Name
drawDescr descr = padLeftRight 1 $
                  strWrap descr

drawCardUI :: State -> Widget Name
drawCardUI s = C.center $
               withBorderStyle BS.unicodeRounded $
               B.border $
               withAttr textboxAttr $
               hLimitPercent 60 $
              --  vLimitPercent 40 $
               drawHeader title <=> drawDescr descr
  where Definition title descr = (s ^. cards) !! (s ^. index)

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') []))         = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))                = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  = halt s
handleEvent s (VtyEvent (V.EvKey V.KEnter []))              = continue $ next s
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
    -- ('#' : xs, '-' : ys) ->
    ('#' : xs, _)        -> Definition (dropWhile isSpace xs) (concat ls)
    _ -> error ("encountered an invalid card: \n" ++ show (lines s))

splitString :: String -> [String]
splitString = splitOn "---"

next :: State -> State
next s = s & index %~ (+1)
