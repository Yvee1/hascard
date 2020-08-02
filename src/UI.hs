module UI (module X, runBrickFlashcards, GlobalState(..), Card) where

import UI.CardSelector as X (addRecent)
import Settings        as X (getUseEscapeCode)
import Runners         as X
import Brick
import Glue
import States
import Types (Card)

runBrickFlashcards :: GlobalState -> IO ()
runBrickFlashcards gs = do
  _ <- defaultMain globalApp gs
  return ()