module Main where

import Lib (runBrickFlashcards)
import BrickHelpers
import Brick
import Brick.Widgets.Center
import CardUI
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import Data.Functor (($>))
import Parser
import System.Environment (getArgs)
import System.Process (runCommand)

main :: IO ()
main = do
  _ <- runCommand "echo -n \\\\e[5 q"
  
  args <- getArgs
  if null args
    then runBrickFlashcards
    else do
      strOrExc <- liftIO (try (readFile (head args)) :: IO (Either IOError String))
      case strOrExc of
        Left exc -> putStr (displayException exc)
        Right str -> case parseCards str of
          Left parseError -> print parseError
          Right result -> runCardUI result $> ()
