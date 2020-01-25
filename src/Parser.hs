module Parser where
  
import Text.Parsec
-- import qualified Data.List.NonEmpty as NE

data Type = Incorrect | Correct
  deriving Show
data CorrectOption = CorrectOption Int String
  deriving Show
newtype IncorrectOption = IncorrectOption String
  deriving Show
data Answer = Answer Type String
  deriving Show

--                         Pre    Gap    Post
data Sentence = Perforated String String Sentence
              | Normal String
  deriving Show
data Perforated = P String String Sentence
  deriving Show

--                     Word   Description
data Card = Definition String String
          | OpenQuestion String Perforated
          | MultipleChoice String CorrectOption [IncorrectOption]
          -- | MultipleAnswer String (NE.NonEmpty Answer) 
  deriving Show

uncurry3 f (a, b, c) = f a b c

parseCards :: String -> Either ParseError [Card]
parseCards = parse cards "failed when parsing cards"
  where
    cards = card `sepEndBy` seperator
    card =  uncurry3 MultipleChoice<$> try pMultChoice
        <|> uncurry Definition <$> pDef
      --  <|> uncurry OpenQuestion <$> p_open

    pHeader = do
      many eol
      char '#'
      spaces
      many notEOL

    pDef = do
      header <- pHeader
      many eol
      descr <- manyTill anyChar $ lookAhead (try seperator)
      return (header, descr)

    pMultChoice = do
      header <- pHeader
      many eol
      options <- many1 pChoice
      let (correct, incorrects) = makeMultipleChoice options
      return (header, correct, incorrects)

    pChoice = do
      kind <- oneOf "*-"
      space
      text <- many (noneOf "*-") 
      return (kind, text)

    eol =  try (string "\n\r")
       <|> try (string "\r\n")
       <|> string "\n"
       <|> string "\r"
       <?> "end of line"

    seperator = string "---"

    notEOL = noneOf "\n\r"


makeMultipleChoice :: [(Char, String)] -> (CorrectOption, [IncorrectOption])
makeMultipleChoice options = makeMultipleChoice' [] [] 0 options
  where
    makeMultipleChoice' [] _ _ [] = error ("multiple choice had no correct answer: \n" ++ show options)
    makeMultipleChoice' [c] ics _ [] = (c, reverse ics)
    makeMultipleChoice' _ _ _ [] = error ("multiple choice had multiple correct answers: \n" ++ show options)
    makeMultipleChoice' cs ics i (('-', text) : opts) = makeMultipleChoice' cs (IncorrectOption text : ics) (i+1) opts
    makeMultipleChoice' cs ics i (('*', text) : opts) = makeMultipleChoice' (CorrectOption i text : cs) ics (i+1) opts
