module DeckHandling where
import Control.Monad (forM)
import qualified Data.List.NonEmpty as NE
import Data.Random
import Lens.Micro.Platform
import States
import Types

doRandomization :: GlobalState -> Bool -> [Card] -> IO ([Int], [Card])
doRandomization gs shuffleAnswers cards = do
    let ixs = [0..length cards - 1]
    shuffledIxs <- if gs^.parameters.pShuffle then sampleFrom (gs^.mwc) (shuffle ixs) else return ixs
    let cards' = map (cards !!) shuffledIxs
    cards'' <- if shuffleAnswers
      then sampleFrom (gs^.mwc) $ mapM shuffleCard cards'
      else return cards'
    return $ (shuffledIxs, cards'')

shuffleCard :: Card -> RVar Card
shuffleCard (c@MultipleAnswer{}) = do
  shuffledOptions <- shuffle . NE.toList $ options c
  return $ c { options = NE.fromList shuffledOptions }
shuffleCard (c@MultipleChoice{}) = do
  let CorrectOption ic sc = correct c
      ixs = [0..length (incorrects c)]
  shuffledIxs <- shuffle ixs
  let ic' = shuffledIxs !! ic
      corrOpt = CorrectOption ic' sc
      incOpts = map (\i -> (incorrects c !!) $ if i > ic' then i - 1 else i) (filter (/= ic') shuffledIxs)
  return $ c { correct = corrOpt, incorrects = incOpts } 
shuffleCard c = return c

doChunking :: Chunk -> [a] -> [a]
doChunking (Chunk i n) cards = 
  splitIntoNChunks n cards !! (i-1)

-- Split into chunks that differ a maximum of 1 in size;
-- the larger chunks are all at the front.
splitIntoNChunks :: Int -> [a] -> [[a]]
splitIntoNChunks n xs =
  let (q, r) = length xs `quotRem` n
      qs = replicate n q
      rs = replicate r 1 ++ repeat 0
      chunkSizes = zipWith (+) qs rs
  in makeChunksOfSizes chunkSizes xs

makeChunksOfSizes :: [Int] -> [a] -> [[a]]
makeChunksOfSizes [] _ = []
makeChunksOfSizes (n:ns) xs = 
  let (chunk, rest) = splitAt n xs
  in chunk : makeChunksOfSizes ns rest