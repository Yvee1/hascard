{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures, OverloadedStrings #-}
module Parser (parseCards) where
  
import Control.Arrow
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Wrap
import Data.Text (pack, unpack)
import Types
import qualified Data.List.NonEmpty as NE

-- Type synonyms for convenience
type Parser = Parsec Void String
type CardParser = Parser (Either String Card)

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
     <|> try pMultAnswer
     <|> try pReorder
     <|> try pOpen
     <|> pDef

pHeader :: Parser String
pHeader = do
  many eol
  char '#'
  spaceChar
  many (noneOf ['\n', '\r'])

pImage :: Parser Image
pImage = do
  many eol
  char '!'
  char '['
  alt <- manyTill anySingle (char ']')
  char '('
  img <- manyTill anySingle (char ')')
  return $ Image alt img

pMaybeImage :: Parser (Maybe Image)
pMaybeImage =  Just <$> try pImage
           <|> pure Nothing

pMultChoice :: CardParser
pMultChoice = do
  header <- pHeader
  img <- pMaybeImage
  many eol
  choices <- pChoice `sepBy1` lookAhead (try choicePrefix)
  msgOrResult <- makeMultipleChoice choices
  case msgOrResult of
    Left errMsg -> do pos <- getSourcePos
                      return . Left $ sourcePosPretty pos <> "\n" <> errMsg
    Right (correct, incorrects) -> return . Right $ MultipleChoice header img correct incorrects

pChoice :: Parser (Char, String)
pChoice = do
  kind <- oneOf ['*','-']
  spaceChar
  text <- manyTill anySingle $ lookAhead (try (try choicePrefix <|> seperator <|> eof'))
  return (kind, text)

choicePrefix :: Parser String
choicePrefix =  string "- "
            <|> string "* "

pMultAnswer :: CardParser
pMultAnswer = do
  header <- pHeader
  img <- pMaybeImage
  many eol
  options <- pOption `sepBy1` lookAhead (try (char '['))
  return . Right $ MultipleAnswer header img (NE.fromList options)

pOption :: Parser Option
pOption = do
  char '['
  kind <- oneOf ['*','x',' ']
  string "] "
  text <- manyTill anySingle $ lookAhead (try (seperator <|> string "[" <|> eof'))
  return $ makeOption kind (dropWhileEnd isSpace' text)

pReorder :: CardParser
pReorder = do
  header <- pHeader
  img <- pMaybeImage
  many eol
  elements <- pReorderElement `sepBy1` lookAhead (try pReorderPrefix)
  let numbers = map fst elements
  if all (`elem` numbers) [1..length numbers]
    then return . Right $ Reorder header img (NE.fromList elements)
    else do pos <- getSourcePos
            return . Left $ sourcePosPretty pos <> "\n" <> "A reordering question should have numbers starting from 1 and increase from there without skipping any numbers, but this is not the case:\n" 
                    <> unlines (map show numbers)

pReorderElement :: Parser (Int, String)
pReorderElement = do
  int <- pReorderPrefix
  text <- manyTill anySingle $ lookAhead (try (try seperator <|> try pReorderPrefix <|> eof'))
  return (read int, dropWhileEnd isSpace' text)

pReorderPrefix :: Parser String
pReorderPrefix = do
  int <- some digitChar
  string ". "
  return int

pOpen :: CardParser
pOpen = do
  header <- pHeader
  img <- pMaybeImage
  many eol
  (pre, gap) <- pGap
  sentence <- pSentence

  return $ Right (OpenQuestion header img (P pre gap sentence))

pSentence :: Parser Sentence
pSentence =  try pPerforated
         <|> pNormal

pPerforated :: Parser Sentence
pPerforated = do
  (pre, gap) <- pGap
  Perforated pre gap <$> pSentence 

chars = try escaped <|> anySingle
escaped = char '\\' >> char '_'

pGap :: Parser (String, NE.NonEmpty String)
pGap = do
  pre <- manyTill chars $ lookAhead (try (string "_" <|> seperator))
  char '_'
  gaps <- manyTill (noneOf ['_','|']) (lookAhead (try gappedSpecialChars)) `sepBy1` string "|"
  char '_'
  return (pre, NE.fromList gaps)

gappedSpecialChars =  seperator
                  <|> string "|"
                  <|> string "_"

pNormal :: Parser Sentence
pNormal = do
  text <- manyTill (noneOf ['_']) $ lookAhead $ try $ seperator <|> eof'
  return (Normal (dropWhileEnd isSpace' text))

pDef :: CardParser
pDef = do
  header <- pHeader
  img <- pMaybeImage
  many eol
  descr <- manyTill chars $ lookAhead $ try $ seperator <|> eof'
  return $ Right (Definition header img (dropWhileEnd isSpace' descr))

eof' = eof >> return [] <?> "end of file"

seperator = do
  sep <- string "---"
  many eol
  return sep

makeMultipleChoice :: [(Char, String)] -> Parser (Either String (CorrectOption, [IncorrectOption]))
makeMultipleChoice options = makeMultipleChoice' [] [] 0 options
  where
    -- makeMultipleChoice' [] _ _ [] = Left ("multiple choice had no correct answer: \n" ++ showPretty options)
    makeMultipleChoice' :: [CorrectOption] -> [IncorrectOption] -> Int -> [(Char, String)] -> Parser (Either String (CorrectOption, [IncorrectOption]))
    makeMultipleChoice' [] _ _ [] = fail "woops"
    makeMultipleChoice' [c] ics _ [] = return $ Right (c, reverse ics)
    makeMultipleChoice' _ _ _ [] = return $ Left ("multiple choice had multiple correct answers: \n" ++ showPretty options)
    makeMultipleChoice' cs ics i (('-', text) : opts) = makeMultipleChoice' cs (IncorrectOption (dropWhileEnd isSpace' text) : ics) (i+1) opts
    makeMultipleChoice' cs ics i (('*', text) : opts) = makeMultipleChoice' (CorrectOption i (dropWhileEnd isSpace' text) : cs) ics (i+1) opts
    makeMultipleChoice' _  _   _ _ = return $ Left "impossible"

    showPretty :: [(Char, String)] -> String
    showPretty = foldr ((<>) . showOne) ""

    showOne (c, s) = [c] <> " " <>  s

makeOption :: Char -> String -> Option
makeOption kind text
  | kind `elem` ['*','x'] = Option Correct text
  | otherwise             = Option Incorrect text

isSpace' :: Char -> Bool
isSpace' '\r' = True
isSpace' a    = isSpace a
