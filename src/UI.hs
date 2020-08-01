module UI (module X, runBrickFlashcards) where

import UI.Cards        as X (runCardsUI)
import UI.CardSelector as X
import UI.MainMenu     as X (runMainMenuUI)
import UI.Settings     as X
import Types           as X (mwc, doShuffle, subset, Card)
import GlobalState     as X (GlobalState (..))
import GlobalState          (globalApp)

runBrickFlashcards :: GlobalState -> IO ()
runBrickFlashcards gs = do
  defaultMain globalApp gs