{-# LANGUAGE TemplateHaskell #-}
module Main where

import UI
import Control.Exception (displayException, try)
import Control.Monad (void, when)
import Data.Either.Extra (mapLeft)
import Data.Version (showVersion)
import Data.Void
import Export
import Lens.Micro.Platform
import Paths_hascard (version)
import Parser
import Options.Applicative hiding (many)
import System.Directory (makeAbsolute)
import System.FilePath (takeExtension, takeBaseName, takeDirectory)
import System.Process (runCommand)
import System.Random.MWC (createSystemRandom)
import Text.Megaparsec (parse, many, errorBundlePretty, ParseErrorBundle)
import Text.Megaparsec.Char.Lexer (charLiteral)
import qualified Data.Map.Strict as Map (empty)
import qualified System.Directory as D
import qualified Stack

data Command = Command
  | Run RunOpts
  | Import ImportOpts
  | Export ExportOpts

data Opts = Opts
  { _optCommand      :: Maybe Command
  , _optVersion      :: Bool
  }

data RunOpts = RunOpts
  { _optFile         :: Maybe String
  , _optSubset       :: Int
  , _optChunk        :: Chunk
  , _optShuffle      :: Bool
  , _optBlankMode    :: Bool
  }

makeLenses ''Opts
makeLenses ''RunOpts
makeLenses ''ImportOpts

main :: IO ()
main = do
  useEscapeCode <- getUseEscapeCode
  when useEscapeCode $ void (runCommand "echo -n \\\\e[5 q")
  
  options <- execParser optsWithHelp
  case (options ^. optVersion, options ^. optCommand) of
    (True, _)                -> putStrLn (showVersion version)
    (_, Just (Run rOpts))    -> run rOpts
    (_, Just (Import iOpts)) -> doImport iOpts
    (_, Just (Export eOpts)) -> doExport eOpts
    (_, Nothing)             ->
      do g <- createSystemRandom 
         let gs = GlobalState {_mwc=g, _states=Map.empty, _stack=Stack.empty, _parameters= defaultParameters }
         start Nothing gs

opts :: Parser Opts
opts = Opts
  <$> optional (hsubparser
    (  command "run" (info (Run <$> runOpts) ( progDesc "Run hascard with CLI options"))
    <> command "import" (info (Import <$> importOpts) (progDesc "Convert a delimited text file (e.g. exported from Quizlet) to a file compatible with hascard.\
    \ The delimiters can be specified via CLI options. By default, terms and definitions are assumed to be separated by tabs, and different cards by new lines.\
    \ Either 'Definition' cards are generated (traditional flashcards), or 'Open question' cards (where the answer needs to be typed).\
    \ For 'Open question' cards, a delimiter can be specified that separates multiple correct answers."))
    <> command "export" (info (Export <$> exportOpts) (progDesc "Convert hascard cards to a delimited text file. Only definition and open question cards can be exported"))))
  <*> switch (long "version" <> short 'v' <> help "Show version number")

runOpts :: Parser RunOpts
runOpts = RunOpts
  <$> optional (argument str (metavar "FILE" <> help "A .txt or .md file containing flashcards"))
  <*> option auto (long "amount" <> short 'a' <> metavar "n" <> help "Use the first n cards in the deck (most useful combined with shuffle)" <> value (-1))
  <*> option auto (long "chunk" <> short 'c' <> metavar "i/n" <> help "Split the deck into n chunks, and review the i'th one. Counting starts at 1." <> value (Chunk 1 1))
  <*> switch (long "shuffle" <> short 's' <> help "Randomize card order")
  <*> switch (long "blank" <> short 'b' <> help "Disable review mode: do not keep track of which questions were correctly and incorrectly answered")

importOpts :: Parser ImportOpts
importOpts = ImportOpts
  <$> argument str (metavar "INPUT" <> help "A delimited text file")
  <*> argument str (metavar "DESINATION" <> help "The filename/path to which the output should be saved")
  <*> option auto (long "type" <> short 't' <> metavar "'open' or 'def'" <> help "The type of card to which the input is transformed; default: open." <> value Open)
  <*> switch (long "reverse" <> short 'r' <> help "Reverse direction of question and answer, i.e. right part becomes the question.")
  <*> strOption (long "row-delimiter" <> metavar "delimiter" <> help "The delimiter used to separate different cards; default: \\n." <> value "\n")
  <*> strOption (long "term-def-delimiter" <> metavar "delimiter" <> help "The delimiter used to separate terms and definitions; default: \\t." <> value "\t")
  <*> optional (strOption (long "def-delimiter" <> metavar "delimiter" <> help "The delimiter used to separate different definitions for the same term; no delimiter is used by default."))

exportOpts :: Parser ExportOpts
exportOpts = ExportOpts
  <$> argument str (metavar "INPUT" <> help "A file containing hascard cards")
  <*> argument str (metavar "DESINATION" <> help "The filename/path to which the output should be saved")
  <*> strOption (long "card-delimiter" <> metavar "delimiter" <> help "The delimiter used to separate different cards; default: \\n." <> value "\n")
  <*> strOption (long "question-delimiter" <> metavar "delimiter" <> help "The delimiter used to separate terms and definitions; default: ,." <> value ",")

optsWithHelp :: ParserInfo Opts
optsWithHelp = info (opts <**> helper) $
              fullDesc <> progDesc "Run the normal application with `hascard`. To run directly on a file, and with CLI options, see `hascard run --help`.\
              \ For converting delimited text files, see `hascard import --help`."
              <> header "Hascard - a TUI for reviewing notes"

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
  | p a = Nothing
  | otherwise = Just a

mkGlobalState :: RunOpts -> GenIO -> GlobalState
mkGlobalState opts gen = 
  let ps = Parameters { _pShuffle = opts^.optShuffle, _pSubset = nothingIf (<0) (opts^.optSubset)
                      , _pChunk = opts^.optChunk, _pReviewMode = not (opts^.optBlankMode), _pOk = False }
  in GlobalState {_mwc=gen, _states=Map.empty, _stack=Stack.empty, _parameters=ps }

cleanFilePath :: FilePath -> IO (Either String FilePath)
cleanFilePath fp = case takeExtension fp of
  ".txt" -> return $ Right fp
  ".md"  -> return $ Right fp
  ""     -> do existence <- mapM D.doesFileExist [fp <> ".txt", fp <> ".md"]
               return $ case existence of
                 [True, True] -> Left "Both a .txt and .md file of this name exist, and it is unclear which to use. Specify the file extension."
                 [True, _   ] -> Right $ fp <> ".txt"     
                 [_   , True] -> Right $ fp <> ".md"
                 _            -> Left $ "No .txt or .md file with the name \""
                                        <> takeBaseName fp
                                        <> "\" in the directory \""
                                        <> takeDirectory fp
                                        <> "\""
              
  _      -> return $ Left "Incorrect file type, provide a .txt file"
  
run :: RunOpts -> IO ()
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
                   start (Just (fp, result)) (mkGlobalState opts gen)

start :: Maybe (FilePath, [Card]) -> GlobalState -> IO ()
start Nothing gs = runBrickFlashcards (gs `goToState_` mainMenuState)
start (Just (fp, cards)) gs = runBrickFlashcards =<< (gs `goToState_`) <$> cardsWithOptionsState gs fp cards

doImport :: ImportOpts -> IO ()
doImport opts' = do
  let opts = opts' & optRowDelimiter %~ parseStringLiteral
  valOrExc <- try $ readFile (opts ^. optInput) :: IO (Either IOError String)
  case valOrExc of
    Left exc -> putStrLn (displayException exc)
    Right val -> do
      let mCards = parseImportInput opts val
      case mCards of
        Right cards -> do
          writeFile (opts ^. optOutput) . cardsToString $ cards
          putStrLn "Successfully converted the file."
        Left msg -> putStrLn msg

doExport :: ExportOpts -> IO ()
doExport opts' = do
  let opts = opts' & optCardDelimiter %~ parseStringLiteral
                   & optQuestionDelimiter %~ parseStringLiteral
  valOrExc <- try $ readFile (opts ^. optExportInput) :: IO (Either IOError String)
  let eitherResult = do
        val <- mapLeft displayException valOrExc
        cards <- parseCards val
        exportCards opts cards


  case eitherResult of
    Left msg -> putStrLn msg
    Right output -> do
      writeFile (opts ^. optExportOutput) output
      putStrLn "Successfully converted the file."
         
parseStringLiteral :: String -> String
parseStringLiteral s = case parse (many charLiteral) "" s of
  Left errorBundle -> error (errorBundlePretty (errorBundle :: ParseErrorBundle String Void))
  Right result -> result