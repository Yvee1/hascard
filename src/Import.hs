{-# LANGUAGE TemplateHaskell #-}
module Import where
import Control.Monad (void)
import Data.Char (toLower, isSpace)
import Data.List
-- import Data.List.Split
import qualified Data.List.NonEmpty as NE
import Data.Void
import Lens.Micro.Platform
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Types

data ImportType = Def | Open

data ImportOpts = ImportOpts
  { _optInput            :: String 
  , _optOutput           :: String
  , _optImportType       :: ImportType
  , _optImportReverse    :: Bool
  , _optRowDelimiter     :: String
  , _optTermDefDelimiter :: String
  , _optDefDelimiter     :: Maybe String }

makeLenses ''ImportOpts

instance Read ImportType where
  readsPrec _ input =
    case map toLower input of
      xs | "open"       `isPrefixOf` xs -> [(Open, drop 4 xs)]
         | "def"        `isPrefixOf` xs -> [(Def,  drop 3 xs)]
         | "definition" `isPrefixOf` xs -> [(Def, drop 10 xs)]
         | otherwise -> []

type Parser = Parsec Void String

rowDelimiter :: String
rowDelimiter = "\n\n"

termDefDelimiter :: String
termDefDelimiter = "\t"

defDelimiter :: String
defDelimiter = ","

parseImportInput :: ImportOpts -> String -> Either String [Card]
parseImportInput opts s = case parse (pImportInput opts) "failed import parsing" s of
  Left parseErrorBundle -> Left $ errorBundlePretty (parseErrorBundle :: ParseErrorBundle String Void)
  Right cards -> Right cards

pImportInput :: ImportOpts -> Parser [Card]
pImportInput opts = pRow opts `sepEndBy1` (void (try (pRowDelimiter *> eol)) <|> void pRowDelimiter <|> void (many eol) <|> eof)
  where pRowDelimiter = string (opts ^. optRowDelimiter)

pRow :: ImportOpts -> Parser Card
pRow opts =
  let
    pTermDefDelimiter = string (opts ^. optTermDefDelimiter)
    pDefDelimiter = string <$> (opts ^. optDefDelimiter)
    pTerm = manyTill anySingle . lookAhead . try $ pSpecial opts
    pDefs = maybe (fmap (:[]) (pDef opts)) (pDef opts `sepBy`) pDefDelimiter
    defBeforeTerm = opts ^. optImportReverse
  in
    case (defBeforeTerm, opts ^. optImportType) of
      (False, Open) -> do
        term <- pTerm
        pTermDefDelimiter
        defs <- pDefs
        return $ OpenQuestion (filter (/= '\n') term) Nothing (P "" (NE.fromList (map (dropWhile isSpace) defs)) (Normal ""))
      (True, Open) -> do
        defs <- pDefs
        pTermDefDelimiter
        term <- pTerm
        return $ OpenQuestion (filter (/= '\n') term) Nothing (P "" (NE.fromList (map (dropWhile isSpace) defs)) (Normal ""))
      (False, Def) -> do
        term <- pTerm
        pTermDefDelimiter
        def <- pDef opts
        return $ Definition (filter (/= '\n') term) Nothing def
      (True, Def) -> do
        def <- pDef opts
        pTermDefDelimiter
        term <- pTerm
        return $ Definition (filter (/= '\n') term) Nothing def

pDef :: ImportOpts -> Parser String
pDef opts = maybe
  (manyTill anySingle . lookAhead . try $ pSpecial opts)
  (\pDefDelimiter -> manyTill anySingle . lookAhead . try $ void pDefDelimiter <|> pSpecial opts)
  (string <$> (opts ^. optDefDelimiter))

pSpecial :: ImportOpts -> Parser ()
pSpecial opts = void pTermDefDelimiter <|> void pRowDelimiter <|> (eol *> eof) <> eof
  where pTermDefDelimiter = string (opts ^. optTermDefDelimiter)
        pRowDelimiter = string (opts ^. optRowDelimiter)
