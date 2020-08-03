module DeckHandling where
import Data.Random
import Lens.Micro.Platform
import States
import Types

doRandomization :: GlobalState -> [Card] -> IO [Card]
doRandomization state cards = 
  let n = length cards in do
    cards' <- if state^.doShuffle then sampleFrom (state^.mwc) (shuffleN n cards) else return cards
    return $ maybe cards' (`take` cards') (state^.subset)