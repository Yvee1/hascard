{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures, OverloadedStrings #-}
module Parser (parseCards) where
  
import Control.Arrow
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Wrap
import Data.Text (pack, unpack)
import Types
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void String

uncurry3 f (a, b, c) = f a b c

parseCards :: String -> Either String [Card]
parseCards s = case parse pCards "failed when parsing cards" s of
  Left parseErrorBundle -> Left $ errorBundlePretty (parseErrorBundle :: ParseErrorBundle String Void)
  Right msgOrCards -> left wrap (sequence msgOrCards)
    where wrap = unlines . map unpack . wrapTextToLines (WrapSettings {preserveIndentation=False, breakLongWords=True}) 40 . pack

pCards :: Parser [Either String Card]
pCards = (pCard `sepEndBy1` seperator) <* eof

pCard :: Parser (Either String Card)
pCard =  try pMultChoice
     <|> Right . uncurry MultipleAnswer <$> try pMultAnswer
     <|> try pReorder
     <|> Right . uncurry OpenQuestion <$> try pOpen
     <|> Right . uncurry Definition <$> pDef

pHeader = do
  many eol
  char '#'
  spaceChar
  many (noneOf ['\n', '\r'])

pMultChoice = do
  header <- pHeader
  many eol
  choices <- pChoice `sepBy1` lookAhead (try choicePrefix)
  case makeMultipleChoice choices of
    Left errMsg -> do pos <- getSourcePos
                      return . Left $ sourcePosPretty pos <> "\n" <> errMsg
    Right (correct, incorrects) -> return . Right $ MultipleChoice header correct incorrects

pChoice = do
  kind <- oneOf ['*','-']
  spaceChar
  text <- manyTill anySingle $ lookAhead (try (try choicePrefix <|> seperator <|> eof'))
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
  kind <- oneOf ['*','x',' ']
  string "] "
  text <- manyTill anySingle $ lookAhead (try (seperator <|> string "[" <|> eof'))
  return $ makeOption kind text

pReorder = do
  header <- pHeader
  many eol
  elements <- pReorderElement `sepBy1` lookAhead (try pReorderPrefix)
  let numbers = map fst elements
  if all (`elem` numbers) [1..length numbers]
    then return . Right $ Reorder header (NE.fromList elements)
    else do pos <- getSourcePos
            return . Left $ sourcePosPretty pos <> "\n" <> "A reordering question should have numbers starting from 1 and increase from there without skipping any numbers, but this is not the case:\n" 
                    <> unlines (map show numbers)

pReorderElement = do
  int <- pReorderPrefix
  text <- manyTill anySingle $ lookAhead (try (try seperator <|> try pReorderPrefix <|> eof'))
  return (read int, text)

pReorderPrefix = do
  int <- some digitChar
  string ". "
  return int

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

chars = escaped <|> anySingle
escaped = char '\\' >> char '_'

pGap = do
  pre <- manyTill chars $ lookAhead (try gappedSpecialChars)
  char '_'
  gaps <- manyTill (noneOf ['_','|']) (lookAhead (try gappedSpecialChars)) `sepBy1` string "|"
  char '_'
  return (pre, NE.fromList gaps)

gappedSpecialChars =  seperator
                  <|> string "|"
                  <|> string "_"

pNormal = do
  text <- manyTill (noneOf ['_']) $ lookAhead $ try $ gappedSpecialChars <|> eof'
  return (Normal text)

pDef = do
  header <- pHeader
  many eol
  descr <- manyTill chars $ lookAhead $ try $ seperator <|> eof'
  return (header, descr)

eof' = eof >> return [] <?> "end of file"

seperator = do
  sep <- string "---"
  many eol
  return sep

makeMultipleChoice :: [(Char, String)] -> Either String (CorrectOption, [IncorrectOption])
makeMultipleChoice options = makeMultipleChoice' [] [] 0 options
  where
    makeMultipleChoice' [] _ _ [] = Left ("multiple choice had no correct answer: \n" ++ showPretty options)
    makeMultipleChoice' [c] ics _ [] = Right (c, reverse ics)
    makeMultipleChoice' _ _ _ [] = Left ("multiple choice had multiple correct answers: \n" ++ showPretty options)
    makeMultipleChoice' cs ics i (('-', text) : opts) = makeMultipleChoice' cs (IncorrectOption text : ics) (i+1) opts
    makeMultipleChoice' cs ics i (('*', text) : opts) = makeMultipleChoice' (CorrectOption i text : cs) ics (i+1) opts
    makeMultipleChoice' _  _   _ _ = Left "impossible"

    showPretty :: [(Char, String)] -> String
    showPretty = foldr ((<>) . showOne) ""

    showOne (c, s) = [c] <> " " <> s

makeOption :: Char -> String -> Option
makeOption kind text
  | kind `elem` ['*','x'] = Option Correct text
  | otherwise             = Option Incorrect text