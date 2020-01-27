module Main where

import Lib (runBrickFlashcards)
import System.Environment (getArgs)

main :: IO ()
main = do
  runBrickFlashcards