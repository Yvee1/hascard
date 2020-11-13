module DeckHandling where
import Data.Random
import Lens.Micro.Platform
import States
import Types

doRandomization :: GlobalState -> [a] -> IO [a]
doRandomization gs cards = 
  let n = length cards in do
    cards' <- if gs^.parameters.pShuffle then sampleFrom (gs^.mwc) (shuffleN n cards) else return cards
    return $ maybe cards' (`take` cards') (gs^.parameters.pSubset)

doChunking :: Chunk -> [a] -> [a]
doChunking (Chunk i n) cards = 
  splitIntoNChunks n cards !! (i-1)

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