{-# LANGUAGE TemplateHaskell #-}
module Main where

import UI
import Control.Exception (displayException, try)
import Control.Monad (void, when)
import Data.Functor (($>))
import Data.Version (showVersion)
import Lens.Micro.Platform
import Paths_hascard (version)
import Parser
import Options.Applicative
import System.Directory (makeAbsolute)
import System.FilePath (takeExtension)
import System.Process (runCommand)
import System.Random.MWC (createSystemRandom, GenIO)

data Opts = Opts
  { _optFile    :: Maybe String
  , _optSubset  :: Int
  , _optShuffle :: Bool
  , _optVersion :: Bool
  }

makeLenses ''Opts

main :: IO ()
main = do
  useEscapeCode <- getUseEscapeCode
  when useEscapeCode $ void (runCommand "echo -n \\\\e[5 q")
  
  options <- execParser optsWithHelp
  if options ^. optVersion
    then putStrLn (showVersion version)
    else run options

opts :: Parser Opts
opts = Opts
  <$> optional (argument str (metavar "FILE" <> help "A .txt file containing flashcards"))
  <*> option auto (long "amount" <> short 'a' <> metavar "n" <> help "Use the first n cards in the deck (most useful combined with shuffle)" <> value (-1))
  <*> switch (long "shuffle" <> short 's' <> help "Randomize card order")
  <*> switch (long "version" <> short 'v' <> help "Show version number")

optsWithHelp :: ParserInfo Opts
optsWithHelp = info (opts <**> helper) $
              fullDesc <> progDesc "Run the normal application without argument, or run it directly on a deck of flashcards by providing a text file."
              <> header "Hascard - a TUI for reviewing notes"

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
  | p a = Nothing
  | otherwise = Just a

run :: Opts -> IO ()
run opts = run' (opts ^. optFile)
  where
    mkGlobalState gen = GlobalState {_mwc=gen, _doShuffle=opts^.optShuffle, _subset=nothingIf (<0) (opts^.optSubset) }
    run' Nothing = createSystemRandom >>= runBrickFlashcards . mkGlobalState
    run' (Just file) = do
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
              Right result -> 
                do gen <- createSystemRandom
                   (makeAbsolute textfile >>= addRecent) *> runCardsWithOptions (mkGlobalState gen) result $> ()