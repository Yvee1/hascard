module Main where

import Lib (runBrickFlashcards)
import BrickHelpers
import Brick
import Brick.Widgets.Center
import System.Environment (getArgs)
import System.Process (runCommand)

main :: IO ()
main = do
  _ <- runCommand "echo \\\\e[5 q"
  runBrickFlashcards

-- test :: Widget ()
-- test = hCenter $ myStrWrap "Heey, this is a very long string of text, well maybe not that long... but pretty long in any case."

-- main :: IO ()
-- main = simpleMain test