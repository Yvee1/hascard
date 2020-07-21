{-# LANGUAGE TemplateHaskell #-}
module Types where
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro.Platform
import System.Random.MWC (GenIO)

data GlobalState = GlobalState
  { _mwc :: GenIO
  , _doShuffle :: Bool
  , _subset :: Maybe Int }

makeLenses ''GlobalState

--                     Word   Description
data Card = Definition String String
          | OpenQuestion String Perforated
          | MultipleChoice {
            mcQuestion   :: String,
            mcCorrect    :: CorrectOption,
            mcIncorrects :: [IncorrectOption]}
          -- | MultipleAnswer String (NE.NonEmpty Answer) 
          | MultipleAnswer {
            maQuestion   :: String,
            maOptions    :: NonEmpty Option }
          
  deriving Show

data Type = Incorrect | Correct
  deriving (Show, Eq)
data CorrectOption = CorrectOption Int String
  deriving Show
newtype IncorrectOption = IncorrectOption String
  deriving Show
data Option = Option Type String
  deriving Show

--                         Pre    Gap               Post
data Sentence = Perforated String (NonEmpty String) Sentence
              | Normal String
  deriving Show

data Perforated = P String (NonEmpty String) Sentence
  deriving Show

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