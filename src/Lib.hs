module Lib where
import CardUI (runCardUI, State)
import MainMenuUI (runMainMenuUI)

runBrickFlashcards :: String -> IO ()
runBrickFlashcards str = do
  _ <- runMainMenuUI
  finalState <- runCardUI str
  pure ()