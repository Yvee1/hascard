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

nGapsInSentence :: Sentence -> Int
nGapsInSentence = nGapsInSentence' 0
nGapsInSentence' acc (Normal s) = acc
nGapsInSentence' acc (Perforated pre gap post) = nGapsInSentence' (1+acc) post

foldSentence :: (String -> a) -> (String -> String -> a -> a) -> Sentence -> a
foldSentence norm perf = f where
  f (Normal text) = norm text
  f (Perforated pre post sent) = perf pre post (f sent)

foldSentenceIndex :: (String -> Int -> a) -> (String -> String -> a -> Int -> a) -> Sentence -> a
foldSentenceIndex norm perf = f 0 where
  f i (Normal text) = norm text i
  f i (Perforated pre post sent) = perf pre post (f (i+1) sent) i

data Perforated = P String String Sentence
  deriving Show

perforatedToSentence :: Perforated -> Sentence
perforatedToSentence (P pre gap sentence) = Perforated pre gap sentence

nGapsInPerforated :: Perforated -> Int
nGapsInPerforated = nGapsInSentence . perforatedToSentence

--                     Word   Description
data Card = Definition String String
          | OpenQuestion String Perforated
          | MultipleChoice {
            mcQuestion   :: String,
            mcCorrect    :: CorrectOption,
            mcIncorrects :: [IncorrectOption]}
          -- | MultipleAnswer String (NE.NonEmpty Answer) 
  deriving Show

example = "# ipRGC\n\
\intrinsically photosensitive Retinal Ganglion Cell\n\
\\n\
\---\n\
\# Retina\n\
\Part of the eye that turns light into electrical neural impulses.\n\
\\n\
\---\n\
\\n\
\# Multiple choice question, (only one answer is right)\n\
\- Option 1\n\
\* Option 2 (this is the correct answer)\n\
\- Option 3\n\
\- Option 4\n\
\\n\
\---"

uncurry3 f (a, b, c) = f a b c

parseCards :: String -> Either ParseError [Card]
parseCards = parse pCards "failed when parsing cards"

pCards = pCard `sepEndBy` seperator
pCard =  uncurry3 MultipleChoice<$> try pMultChoice
     <|> uncurry OpenQuestion <$> try pOpen
     <|> uncurry Definition <$> pDef

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
  options <- pChoice `sepBy1` lookAhead (try annoyance)
  let (correct, incorrects) = makeMultipleChoice options
  return (header, correct, incorrects)

pChoice = do
  kind <- oneOf "*-"
  space
  text <- many (noneOf "*-")
  return (kind, text)

annoyance =  string "- "
         <|> string "* "

pOpen = do
  header <- pHeader
  many eol
  (pre, gap) <- pGap
  sentence <- pSentence

  return (header, P pre gap sentence)

pSentence =  try pPerforated
         <|> pNormal
  
pPerforated = do
  (pre, gap) <- pGap
  Perforated pre gap <$> pSentence 
  
pGap = do
  pre <- manyTill anyChar $ lookAhead (try annoyance2)
  char '_'
  gap <- manyTill (noneOf "_") $ lookAhead (try annoyance2)
  char '_'
  return (pre, gap)

annoyance2 =  seperator
          <|> string "_"

pNormal = do
  text <- manyTill (noneOf "_") $ lookAhead (try annoyance2)
  return (Normal text)

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
