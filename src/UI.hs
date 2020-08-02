module UI (module X, runBrickFlashcards, GlobalState(..), Card) where

import UI.Cards        as X (runCardsUI, runCardsWithOptions)
import UI.CardSelector as X (addRecent)
import UI.MainMenu     as X (runMainMenuUI)
import UI.Settings     as X (getUseEscapeCode)
-- import Types           as X (mwc, doShuffle, subset, Card)
-- import GlobalState     as X (GlobalState (..))
-- import GlobalState          (globalApp)
import Brick
import Glue
import States
import Types (Card)

runBrickFlashcards :: GlobalState -> IO ()
runBrickFlashcards gs = do
  _ <- defaultMain globalApp gs
  return ()