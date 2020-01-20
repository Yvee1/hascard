module Lib where

import Data.List.Split
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char

handleFilePath :: FilePath -> IO String
handleFilePath = readFile

--                     Word   Description
data Card = Definition String String
  deriving Show

runFlashcards :: String -> IO ()
runFlashcards input = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let cards = stringToCards input
  drawCards cards
  quit 

stringToCards :: String -> [Card]
stringToCards = map stringToCard . splitString

stringToCard :: String -> Card
stringToCard s = let (firstLine : rest) = dropWhile (`elem` ["\n", "\r\n", "\r", ""]) (lines s) in
  case firstLine of
    ('#' : xs) -> Definition (dropWhile isSpace xs) (concat rest)
    _ -> error ("encountered an invalid card: \n" ++ show (lines s))

splitString :: String -> [String]
splitString = splitOn "---"

drawCard :: Card -> IO ()
drawCard (Definition word def) = do
  cls
  goto (1, 1)
  putStr "\ESC[1;4m"
  putStrLn word
  putStr "\ESC[0m"
  putStrLn def

drawCards :: [Card] -> IO ()
drawCards [] = pure ()
drawCards (c : cs) = do
  drawCard c
  next cs
  
next :: [Card] -> IO ()
next cs = do cmd <- getKey
             case cmd of
               "q" -> quit
               "\ESC" -> quit
               "\n" -> drawCards cs
               _ -> next cs

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- from https://stackoverflow.com/a/38553473/11931091
getKey :: IO String
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

quit :: IO ()
quit = do
  cls
  goto (1, 1)