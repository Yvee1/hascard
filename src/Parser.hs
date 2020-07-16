-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures #-}
module Parser (parseCards) where
  
import qualified Data.List.NonEmpty as NE
import Text.Parsec
import Types

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
     <|> uncurry MultipleAnswer <$> try pMultAnswer
     <|> uncurry OpenQuestion <$> try pOpen
     <|> uncurry Definition <$> pDef

pHeader = do
  many eol
  char '#'
  spaces
  many notEOL

pMultChoice = do
  header <- pHeader
  many eol
  choices <- pChoice `sepBy1` lookAhead (try choicePrefix)
  let (correct, incorrects) = makeMultipleChoice choices
  return (header, correct, incorrects)

pChoice = do
  kind <- oneOf "*-"
  space
  text <- manyTill anyChar $ lookAhead (try (try choicePrefix <|> seperator))
  return (kind, text)

choicePrefix =  string "- "
            <|> string "* "

pMultAnswer = do
  header <- pHeader
  many eol
  options <- pOption `sepBy1` lookAhead (try (char '['))
  return (header, NE.fromList options)

pOption = do
  char '['
  kind <- oneOf "*x "
  string "] "
  text <- manyTill anyChar $ lookAhead (try (seperator <|> string "["))
  return $ makeOption kind text

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
  pre <- manyTill anyChar $ lookAhead (try gappedSpecialChars)
  char '_'
  gap <- manyTill (noneOf "_") $ lookAhead (try gappedSpecialChars)
  char '_'
  return (pre, gap)

gappedSpecialChars =  seperator
                  <|> string "_"

pNormal = do
  text <- manyTill (noneOf "_") $ lookAhead (try gappedSpecialChars)
  return (Normal text)

pDef = do
  header <- pHeader
  many eol
  descr <- manyTill anyChar $ lookAhead (try seperator)
  return (header, descr)

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

makeOption :: Char -> String -> Option
makeOption kind text
  | kind `elem` "*x" = Option Correct text
  | otherwise        = Option Incorrect text