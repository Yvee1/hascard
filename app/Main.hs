module Main where

import Lib (runBrickFlashcards)
import System.Environment (getArgs)
import BrickHelpers
import Brick
import Brick.Widgets.Center

main :: IO ()
main = runBrickFlashcards

-- test :: Widget ()
-- test = hCenter $ myStrWrap "Heey, this is a very long string of text, well maybe not that long... but pretty long in any case."

-- main :: IO ()
-- main = simpleMain test