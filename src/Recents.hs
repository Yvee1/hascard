module Recents where
import Control.Monad (filterM)
import Data.List (sort)
import Settings
import Stack (Stack)
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitFileName, dropExtension, splitPath, joinPath)
import qualified Stack as S
import qualified System.Directory as D
import qualified System.IO.Strict as IOS (readFile)

getRecents :: IO (Stack FilePath)
getRecents = do
  rf <- getRecentsFile
  exists <- D.doesFileExist rf
  if exists
    then removeDeletedFiles rf *> clampRecents rf
    else return S.empty

removeDeletedFiles :: FilePath -> IO (Stack FilePath)
removeDeletedFiles fp = do
  contents <- IOS.readFile fp
  existing <- S.fromList <$> filterM D.doesFileExist (lines contents)
  writeRecents existing
  return existing

parseRecents :: String -> Stack FilePath
parseRecents = S.fromList . lines

clampRecents :: FilePath -> IO (Stack FilePath)
clampRecents fp = do
  rs <- parseRecents <$> IOS.readFile fp
  maxRs <- getMaxRecents
  let clamped = S.takeStack maxRs rs
  writeRecents clamped
  return clamped

addRecent :: FilePath -> IO ()
addRecent fp = do
  rs <- getRecents
  maxRecents <- getMaxRecents
  let rs'  = fp `S.insert` rs 
      rs'' = if S.size rs' <= maxRecents
              then rs'
              else S.removeLast rs'
  writeRecents rs''

writeRecents :: Stack FilePath -> IO ()
writeRecents stack = do
  file <- getRecentsFile
  writeFile file $ unlines (S.toList stack)

getRecentsFile :: IO FilePath
getRecentsFile = do
  maybeSnap <- lookupEnv "SNAP_USER_DATA"
  xdg <- D.getXdgDirectory D.XdgData "hascard"

  let dir = case maybeSnap of
                Just path | not (null path) -> path
                          | otherwise       -> xdg
                Nothing                     -> xdg
  D.createDirectoryIfMissing True dir

  return (dir </> "recents")

initLast :: [a] -> ([a], a)
initLast [x] = ([], x)
initLast (x:xs) = let (xs', y) = initLast xs
                   in (x:xs', y)

shortenFilepaths :: [FilePath] -> [FilePath]
shortenFilepaths fps = uncurry shortenFilepaths' (unzip (map ((\(pre, fn) -> (pre, dropExtension fn)) . splitFileName) fps))
  where
    shortenFilepaths' prefixes abbreviations =
      let ds = duplicates abbreviations in
        if null ds then abbreviations else
          shortenFilepaths' 
            (flip map (zip [0..] prefixes) (
              \(i, pre) -> if i `elem` ds then
                joinPath (init (splitPath pre)) else pre
            ))
            (flip map (zip [0..] abbreviations) (
              \(i, abbr) -> if i `elem` ds then 
                last (splitPath (prefixes !! i)) ++ abbr
                else abbr) )
          

duplicates :: Eq a => [a] -> [Int]
duplicates = sort . map fst . duplicates' 0 [] []
  where duplicates' _ _    acc []     = acc
        duplicates' i seen acc (x:xs) = duplicates' (i+1) ((i, x) : seen) acc' xs
          where acc' = case (getPairsWithValue x acc, getPairsWithValue x seen) of
                  ([], []) -> acc
                  ([], ys) -> (i, x) : ys ++ acc
                  (_, _)   -> (i, x) : acc
                -- acc' = if getPairsWithValue x seen then (i, x) : acc else acc 

getPairsWithValue :: Eq a => a -> [(Int, a)] -> [(Int, a)]
getPairsWithValue y []       = []
getPairsWithValue y ((i, x):xs)
  | x == y    = (i, x) : getPairsWithValue y xs
  | otherwise = getPairsWithValue y xs
