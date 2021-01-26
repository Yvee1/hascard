module Types where
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty)
import System.FilePath
import System.Process
import System.Info
import System.IO
import qualified Data.List.NonEmpty as NE
import qualified System.Directory as D

--                     Word   Description
data Card = Definition { 
            question   :: String,
            external   :: Maybe External,
            definition :: String }
          | OpenQuestion {
            question   :: String,
            external   :: Maybe External,
            perforated :: Perforated }
          | MultipleChoice {
            question   :: String,
            external   :: Maybe External,
            correct    :: CorrectOption,
            incorrects :: [IncorrectOption]}
          | MultipleAnswer {
            question   :: String,
            external   :: Maybe External,
            options    :: NonEmpty Option }
          | Reorder {
            question   :: String,
            external   :: Maybe External,
            elements   :: NonEmpty (Int, String)
          }

instance Show Card where
  show card = let showHeader h = "# " <> h <> "\n"
    in case card of
      Definition h img descr -> showHeader h <> maybe "" show img <> "\n" <> descr
      OpenQuestion h img p -> showHeader h <> maybe "" show img <> "\n" <> show p
      MultipleChoice h img c inc -> 
        showHeader h <> maybe "" show img <> "\n" <> showMultipleChoice c inc
      MultipleAnswer h img opts ->
        showHeader h <> maybe "" show img <> "\n" <> unlines' (NE.toList (NE.map show opts))
      Reorder h img elts -> 
        showHeader h <>maybe "" show img <> "\n" <> unlines' (NE.toList (NE.map showReorder elts))

--              alt   file
data External = Image String String
              | Latex String

instance Show External where
  show (Image alt file) = "![" <> alt <> "]" <> "(" <> file <> ")"
  show (Latex text) = "```\n" <> text <> "```" 

openCommand :: String
openCommand = case os of
  "darwin" -> "open"
  "linux"  -> "xdg-open"
  _        -> error "Unkown OS for opening images"

openImage :: FilePath -> FilePath -> IO ()
openImage origin relative = openImage' (origin </> relative)

openImage' :: FilePath -> IO ()
openImage' fp = do
  exists <- D.doesFileExist fp 
  if exists
    then void $ runCommand (openCommand <> " \"" <> fp <> "\"")
    else error $ "The image you were trying to open does not exist: " <> fp

openLatex :: String -> IO ()
openLatex latex = do
  let packages = ["amsfonts", "mathtools"]
      text = unlines $
          [ "\\documentclass[preview]{standalone}" ]
          ++ map (\p -> "\\usepackage{"<>p<>"}") packages ++
          [ "\\begin{document}"
          , latex
          , "\\end{document}" ]
  dir <- D.getTemporaryDirectory
  (tempfile, temph) <- openTempFile dir "hascard-latex-"
  hPutStrLn temph text
  hClose temph
  callProcess "pdflatex" ["-output-directory", dir, tempfile]
  openImage' (tempfile <> ".pdf")

openCardExternal :: FilePath -> Card -> IO ()
openCardExternal origin card =
  case external card of
    Nothing -> pure ()
    Just (Image _ relative) -> openImage origin relative
    Just (Latex text) -> openLatex text

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg 

data Type = Incorrect | Correct
  deriving (Show, Eq)
data CorrectOption = CorrectOption Int String
  deriving Show
newtype IncorrectOption = IncorrectOption String
  deriving Show
data Option = Option Type String
instance Show Option where
  show (Option Correct str)   = "[*] " <> str
  show (Option Incorrect str) = "[ ] " <> str

--                         Pre    Gap               Post
data Sentence = Perforated String (NonEmpty String) Sentence
              | Normal String

instance Show Sentence where
  show = foldSentence id (\pre gap sent -> pre <> "_" <> concat (NE.intersperse "|" gap) <> "_" <> sent)

data Perforated = P String (NonEmpty String) Sentence

instance Show Perforated where
  show = show . perforatedToSentence

listMultipleChoice :: CorrectOption -> [IncorrectOption] -> [String]
listMultipleChoice c = reverse . listMultipleChoice' [] 0 c
  where listMultipleChoice' opts i (CorrectOption j cStr) [] = 
          if i == j
            then cStr : opts
            else opts
        listMultipleChoice' opts i c'@(CorrectOption j cStr) ics@(IncorrectOption icStr : ics') = 
          if i == j
            then listMultipleChoice' (cStr  : opts) (i+1) c' ics
            else listMultipleChoice' (icStr : opts) (i+1) c' ics'

unlines' :: [String] -> String
unlines' = intercalate "\n"

showMultipleChoice :: CorrectOption -> [IncorrectOption] -> String
showMultipleChoice c@(CorrectOption i _) inc = 
  unlines' . map showOne $ zip [0..] (listMultipleChoice c inc)
    where showOne (j, s) = (if i == j then "* " else "- ") <> s

showReorder :: (Int, String) -> String
showReorder (i, s) = show i <> ". " <> s

cardsToString :: [Card] -> String
cardsToString = unlines . intersperse "---" . map show

nGapsInSentence :: Sentence -> Int
nGapsInSentence = nGapsInSentence' 0
  where
    nGapsInSentence' acc (Normal _) = acc
    nGapsInSentence' acc (Perforated _ _ post) = nGapsInSentence' (1+acc) post

foldSentence :: (String -> a) -> (String -> NonEmpty String -> a -> a) -> Sentence -> a
foldSentence norm perf = f where
  f (Normal text) = norm text
  f (Perforated pre gap sent) = perf pre gap (f sent)

foldSentenceIndex :: (String -> Int -> a) -> (String -> NonEmpty String -> a -> Int -> a) -> Sentence -> a
foldSentenceIndex norm perf = f 0 where
  f i (Normal text) = norm text i
  f i (Perforated pre gap sent) = perf pre gap (f (i+1) sent) i

perforatedToSentence :: Perforated -> Sentence
perforatedToSentence (P pre gap sentence) = Perforated pre gap sentence

nGapsInPerforated :: Perforated -> Int
nGapsInPerforated = nGapsInSentence . perforatedToSentence

sentenceToGaps :: Sentence -> [NonEmpty String]
sentenceToGaps = foldSentence (const []) (\_ gap acc -> gap : acc)

isOptionCorrect :: Option -> Bool
isOptionCorrect (Option Correct _) = True
isOptionCorrect _                  = False