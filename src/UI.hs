module UI (module X, module UI) where

import UI.Cards        as X (runCardsUI)
import UI.CardSelector as X
import UI.MainMenu     as X (runMainMenuUI)
import UI.Settings     as X
import Types           as X (GlobalState (..), mwc, doShuffle, subset, Card)

runBrickFlashcards :: GlobalState -> IO ()
runBrickFlashcards = runMainMenuUI