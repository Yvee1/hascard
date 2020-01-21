module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      file <- handleFilePath input
      runBrickFlashcards file
    _ -> putStrLn "error: input filepath to a flashcard"
