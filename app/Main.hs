{-# LANGUAGE TemplateHaskell #-}
module Main where

import UI
import Control.Exception (displayException, try)
import Control.Monad (void, when)
import Data.Version (showVersion)
import Lens.Micro.Platform
import Paths_hascard (version)
import Parser
import Options.Applicative
import System.Directory (makeAbsolute)
import System.FilePath (takeExtension)
import System.Process (runCommand)
import System.Random.MWC (createSystemRandom)
import qualified Data.Map.Strict as Map (empty)
import qualified System.Directory as D
import qualified Stack

data Opts = Opts
  { _optFile         :: Maybe String
  , _optSubset       :: Int
  , _optChunk        :: Chunk
  , _optShuffle      :: Bool
  , _optReview       :: Bool
  , _optVersion      :: Bool
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
  <$> optional (argument str (metavar "FILE" <> help "A .txt or .md file containing flashcards"))
  <*> option auto (long "amount" <> short 'a' <> metavar "n" <> help "Use the first n cards in the deck (most useful combined with shuffle)" <> value (-1))
  <*> option auto (long "chunk" <> short 'c' <> metavar "i/n" <> help "Split the deck into n chunks, and review the i'th one. Counting starts at 1." <> value (Chunk 1 1))
  <*> switch (long "shuffle" <> short 's' <> help "Randomize card order")
  <*> switch (long "review" <> short 'r' <> help "Enable review mode: keep track of which questions were correctly and incorrectly answered")
  <*> switch (long "version" <> short 'v' <> help "Show version number")

optsWithHelp :: ParserInfo Opts
optsWithHelp = info (opts <**> helper) $
              fullDesc <> progDesc "Run the normal application without argument, or run it directly on a deck of flashcards by providing a text file. Options work either way."
              <> header "Hascard - a TUI for reviewing notes"

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
  | p a = Nothing
  | otherwise = Just a

mkGlobalState :: Opts -> GenIO -> GlobalState
mkGlobalState opts gen = GlobalState {_mwc=gen, _doShuffle=opts^.optShuffle, _subset=nothingIf (<0) (opts^.optSubset), _states=Map.empty, _stack=Stack.empty, _chunk=opts^.optChunk, _doReview=opts^.optReview }

cleanFilePath :: FilePath -> IO (Either String FilePath)
cleanFilePath fp = case takeExtension fp of
  ".txt" -> return $ Right fp
  ".md"  -> return $ Right fp
  ""     -> do existence <- mapM D.doesFileExist [fp <> ".txt", fp <> ".md"]
               return $ case existence of
                 [True, True] -> Left "Both a .txt and .md file of this name exist, and it is unclear which to use. Specify the file extension."
                 True:_       -> Right $ fp <> ".txt"     
                 _            -> Right $ fp <> ".md"
              
  _      -> return $ Left "Incorrect file type, provide a .txt file"
  
run :: Opts -> IO ()
run opts = run' (opts ^. optFile)
  where
    run' Nothing = createSystemRandom >>= start Nothing . mkGlobalState opts
    run' (Just dirtyFp) = do
      msgOrFp <- cleanFilePath dirtyFp
      case msgOrFp of
        Left msg -> putStrLn msg
        Right fp -> do
          valOrExc <- try (readFile fp) :: IO (Either IOError String)
          case valOrExc of
            Left exc -> putStrLn (displayException exc)
            Right val -> case parseCards val of
              Left parseError -> putStr parseError
              Right result ->
                do gen <- createSystemRandom
                   makeAbsolute fp >>= addRecent
                   start (Just result) (mkGlobalState opts gen)

start :: Maybe [Card] -> GlobalState -> IO ()
start Nothing gs = runBrickFlashcards (gs `goToState` mainMenuState)
start (Just cards) gs = runBrickFlashcards =<< (gs `goToState`) <$> cardsWithOptionsState gs cards