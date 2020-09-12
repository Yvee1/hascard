module UI 
( module X
, runBrickFlashcards

, GlobalState(..)
, GenIO
, Chunk(..)
, Card
, ImportType(..)
, Parameters(..)

, goToState

, cardsToString

, parseImportInput
) where

import UI.CardSelector as X (addRecent)
import Settings        as X (getUseEscapeCode)
import Runners         as X
import Brick
import Glue
import Import
import States
import StateManagement
import Types (Card, cardsToString)

runBrickFlashcards :: GlobalState -> IO ()
runBrickFlashcards gs = do
  _ <- defaultMain globalApp gs
  return ()