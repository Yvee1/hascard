module UI (module X, runBrickFlashcards) where

import UI.Cards    as X (runCardsUI)
import UI.MainMenu as X (runMainMenuUI)
import UI.Settings as X

runBrickFlashcards :: IO ()
runBrickFlashcards = runMainMenuUI
  