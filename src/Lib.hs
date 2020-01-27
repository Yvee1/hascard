module Lib where
import CardUI (runCardUI, State)
import MainMenuUI (runMainMenuUI)
import Brick.Widgets.FileBrowser (FileInfo, fileInfoFilePath)

runBrickFlashcards :: IO ()
runBrickFlashcards = do
  fileInfo <- runMainMenuUI
  str <- readFile $ fileInfoFilePath fileInfo
  finalState <- runCardUI str
  pure ()