module Main where

import UI
import Control.Exception (displayException, try)
import Control.Monad (void, when)
import Data.Functor (($>))
import Data.Version (showVersion)
import Paths_hascard (version)
import Parser
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Process (runCommand)
import System.FilePath (takeExtension)

data Opts = Opts
  { optFile    :: Maybe String
  , optVersion :: Bool
  }

main :: IO ()
main = do
  useEscapeCode <- getUseEscapeCode
  when useEscapeCode $ void (runCommand "echo -n \\\\e[5 q")
  
  options <- execParser optsWithHelp
  if optVersion options
    then putStrLn (showVersion version)
    else run $ optFile options

opts :: Parser Opts
opts = Opts
  <$> optional (argument str (metavar "FILE.txt" <> help "A .txt file containing flashcards"))
  <*> switch (long "version" <> short 'v' <> help "Show version number")

optsWithHelp :: ParserInfo Opts
optsWithHelp = info (opts <**> helper) $
              fullDesc <> progDesc "Run the normal application without argument, or run it directly on a deck of flashcards by providing a text file."
              <> header "Hascard - a TUI for reviewing notes"

run :: Maybe String -> IO ()
run Nothing = runBrickFlashcards
run (Just file) = do
  let filepath = 
        case takeExtension file of
          ""     -> Just $ file <> ".txt"
          ".txt" -> Just file
          _      -> Nothing
  case filepath of
     Nothing -> putStrLn "Incorrect file type, provide a .txt file" 
     Just textfile -> do
       valOrExc <- try (readFile textfile) :: IO (Either IOError String)
       case valOrExc of
         Left exc -> putStrLn (displayException exc)
         Right val -> case parseCards val of
           Left parseError -> print parseError
           Right result -> (makeAbsolute textfile >>= addRecent) *> runCardsUI result $> ()