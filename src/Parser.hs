{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures #-}
module Parser (parseCards) where
  
import qualified Data.List.NonEmpty as NE
import Text.Parsec
import Types

uncurry3 f (a, b, c) = f a b c

parseCards :: String -> Either ParseError [Card]
parseCards = parse pCards "failed when parsing cards"

pCards = pCard `sepEndBy1` seperator
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
  text <- manyTill anyChar $ lookAhead (try (try choicePrefix <|> seperator <|> (eof >> return [])))
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
  text <- manyTill anyChar $ lookAhead (try (seperator <|> string "[" <|> (eof >> return [])))
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

chars = escaped <|> anyChar
escaped = char '\\' >> char '_'

pGap = do
  pre <- manyTill chars $ lookAhead (try gappedSpecialChars)
  char '_'
  gaps <- manyTill (noneOf "_|") (lookAhead (try gappedSpecialChars)) `sepBy1` string "|"
  char '_'
  return (pre, NE.fromList gaps)

gappedSpecialChars =  seperator
                  <|> string "|"
                  <|> string "_"

pNormal = do
  text <- manyTill (noneOf "_") $ lookAhead $ try $ gappedSpecialChars <|> (eof >> return [])
  return (Normal text)

pDef = do
  header <- pHeader
  many eol
  descr <- manyTill chars $ lookAhead (try (seperator <|> (eof >> return [])))
  return (header, descr)

eol =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

seperator = do
  sep <- string "---"
  many eol
  return sep

notEOL = noneOf "\n\r"

makeMultipleChoice :: [(Char, String)] -> (CorrectOption, [IncorrectOption])
makeMultipleChoice options = makeMultipleChoice' [] [] 0 options
  where
    makeMultipleChoice' [] _ _ [] = error ("multiple choice had no correct answer: \n" ++ show options)
    makeMultipleChoice' [c] ics _ [] = (c, reverse ics)
    makeMultipleChoice' _ _ _ [] = error ("multiple choice had multiple correct answers: \n" ++ show options)
    makeMultipleChoice' cs ics i (('-', text) : opts) = makeMultipleChoice' cs (IncorrectOption text : ics) (i+1) opts
    makeMultipleChoice' cs ics i (('*', text) : opts) = makeMultipleChoice' (CorrectOption i text : cs) ics (i+1) opts
    makeMultipleChoice' _  _   _ _ = error "impossible"

makeOption :: Char -> String -> Option
makeOption kind text
  | kind `elem` "*x" = Option Correct text
  | otherwise        = Option Incorrect text