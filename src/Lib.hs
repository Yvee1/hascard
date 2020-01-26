{-# LANGUAGE TemplateHaskell #-}
module Lib where

import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char
import Brick
import Lens.Micro.Platform
import Parser
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

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
listMultipleChoice c = concat . reverse . listMultipleChoice' [] 0 c
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
handleEvent s (VtyEvent (V.EvKey V.KRight []))              = next s
handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') []))         = next s
handleEvent s (VtyEvent (V.EvKey V.KLeft  []))              = previous s
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
  let cards = case parseCards input of
              Left parseError -> error (show parseError)
              Right result -> result 
  let initialState = State cards 0 0
  finalState <- defaultMain app initialState
  pure ()

next :: State -> EventM Name (Next State)
next s = if (s ^. index + 1) < length (s ^. cards)
          then continue $ s & index %~ (+1)
          -- else halt s
          else continue s

previous :: State -> EventM Name (Next State)
previous s | s ^. index > 0 = continue $ s & index %~ subtract 1
           | otherwise      = continue s