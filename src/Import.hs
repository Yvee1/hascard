module Import where
import Data.Char (toLower, isSpace)
import Data.List
import Data.List.Split
import qualified Data.List.NonEmpty as NE
import Types

data ImportType = Def | Open

instance Read ImportType where
  readsPrec _ input =
    case map toLower input of
      xs | "open"       `isPrefixOf` xs -> [(Open, drop 4 xs)]
         | "def"        `isPrefixOf` xs -> [(Def,  drop 3 xs)]
         | "definition" `isPrefixOf` xs -> [(Def, drop 10 xs)]
         | otherwise -> []

parseImportInput :: ImportType -> Bool -> String -> [Card]
parseImportInput iType reverse input = 
  let listToTuple = if not reverse then \[q, a] -> (q, a) else \[a, q] -> (q, a)
      xs = map (listToTuple . splitOn "\t") (lines input)
      makeOpen (header, body) = OpenQuestion header 
        (P "" (NE.fromList (map (dropWhile isSpace) (splitOneOf ",/;" body))) (Normal ""))

  in case iType of
    Def  -> map (uncurry Definition) xs
    Open -> map makeOpen xs 