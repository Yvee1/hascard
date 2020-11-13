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

parseImportInput :: ImportType -> Bool -> String -> Maybe [Card]
parseImportInput iType reverse input = 
  let listToTuple [q, a] = Just $ if not reverse then (q, a) else (a, q)
      listToTuple _ = Nothing
      xs = mapM (listToTuple . splitOn "\t") (lines input)
      makeOpen (header, body) = OpenQuestion header Nothing
        (P "" (NE.fromList (map (dropWhile isSpace) (splitOneOf ",/;" body))) (Normal ""))

  in case iType of
    Def  -> map (\(s1, s2) -> Definition s1 Nothing s2) <$> xs
    Open -> map makeOpen <$> xs 