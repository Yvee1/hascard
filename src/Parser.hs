module Parser where
  
import Text.ParserCombinators.Parsec
import Data.List.Split
import qualified Data.List.NonEmpty as NE

data Type = Incorrect | Correct
  deriving Show
data CorrectOption = CorrectOption Int String
  deriving Show
newtype IncorrectOption = IncorrectOption String
  deriving Show
data Answer = Answer Type String
  deriving Show

--                     Word   Description
data Card = Definition String String
          | MultipleChoice String CorrectOption [IncorrectOption]
          | MultipleAnswer String (NE.NonEmpty Answer) 
  deriving Show

-- cardsFile = sepBy card (char '|')
-- card = endBy line eol
-- line = prefix <|> sentence
-- sentence = many (noneOf "\n\r|")
-- eol = try (string "\n\r")
--   <|> try (string "\r\n")
--   <|> string "\n"
--   <|> string "\r"
--   <?> "end of line"
-- prefix = string "# "
--      <|> string "- "
--      <|> string "* "

-- parseCards :: String -> Either ParseError [[String]]
-- parseCards = parse cardsFile "(unknown)"


--------------------------------

stringToCards :: String -> [Card]
stringToCards = map stringToCard . splitString

stringToCard :: String -> Card
stringToCard s = let (fstLine : ls@(sndLine : rest)) = dropWhile (`elem` ["\n", "\r\n", "\r", ""]) (lines s) in
  case (fstLine, sndLine) of
    ('#' : ' ' : question, '-' : ' ' : _) -> makeMultipleChoice question ls
    ('#' : ' ' : question, '*' : ' ' : _) -> makeMultipleChoice question ls
    ('#' : ' ' : title, _)           -> makeDefinition title ls
    _                           -> error ("encountered an invalid card: \n" ++ show (lines s))

makeDefinition :: String -> [String] -> Card
makeDefinition title descr = Definition title (unlines descr)

makeMultipleChoice :: String -> [String] -> Card
makeMultipleChoice question ls = MultipleChoice question correct incorrects
  where (correct, incorrects) = makeMultipleChoice' [] [] 0 ls
        makeMultipleChoice' [] _ _ [] = error ("multiple choice had no correct answer: \n" ++ unlines (question : ls))
        makeMultipleChoice' [c] ics _ [] = (c, reverse ics)
        makeMultipleChoice' _ _ _ [] = error ("multiple choice had multiple correct answers: \n" ++ unlines (question : ls))
        makeMultipleChoice' cs ics i (('-' : ' ' : opt) : opts) = makeMultipleChoice' cs (IncorrectOption opt : ics) (i+1) opts
        makeMultipleChoice' cs ics i (('*' : ' ' : opt) : opts) = makeMultipleChoice' (CorrectOption i opt : cs) ics (i+1) opts

splitString :: String -> [String]
splitString = splitOn "---"