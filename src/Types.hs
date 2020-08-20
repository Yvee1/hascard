module Types where
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

--                     Word   Description
data Card = Definition String String
          | OpenQuestion String Perforated
          | MultipleChoice {
            question   :: String,
            correct    :: CorrectOption,
            incorrects :: [IncorrectOption]}
          | MultipleAnswer {
            question   :: String,
            options    :: NonEmpty Option }
          | Reorder {
            question   :: String,
            elements   :: NonEmpty (Int, String)
          }

instance Show Card where
  show card = let showHeader h = "# " <> h <> "\n"
    in case card of
      Definition h descr -> showHeader h <> descr
      OpenQuestion h p -> showHeader h <> show p
      MultipleChoice h c inc -> 
        showHeader h <> showMultipleChoice c inc
      MultipleAnswer h opts ->
        showHeader h <> unlines' (NE.toList (NE.map show opts))
      Reorder h elts -> 
        showHeader h <> unlines' (NE.toList (NE.map showReorder elts))

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