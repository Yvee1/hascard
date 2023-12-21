{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Parameters where
import UI.Attributes
import Brick
import Brick.Widgets.Center
import Brick.Forms
import DeckHandling
import Data.Maybe
import Data.Char (isDigit)
import Data.Text (pack)
import Lens.Micro.Platform
import States
import Text.Read (readMaybe)
import UI.BrickHelpers
import qualified Data.Text as T
import qualified Graphics.Vty as V

mkParameterForm :: Int -> Parameters -> Form Parameters e Name
mkParameterForm n ps =
  let label s w = padBottom (Pad 1) $ padRight (Pad 2) (strWrap s) <+> w
      form = newForm
        [ chunkSubsetField n (chunkSubsetLens n)
        , label "Shuffle the deck?" @@= yesnoField True pShuffle ShuffleField ""
        , label "Review mode?" @@= yesnoField True pReviewMode ReviewModeField ""
        , hCenter @@= okField pOk ParametersOkField "Ok"
        ] ps
  in setFormFocus ParametersOkField form

chunkSubsetLens :: Int -> Lens' Parameters (Chunk, Int)
chunkSubsetLens n = lens getter setter
  where getter ps = (ps^.pChunk, fromMaybe n (ps^.pSubset))
        setter ps (c, int) = ps & pChunk.~c & pSubset ?~ int

chunkSubsetField :: Int -> Lens' s (Chunk, Int) -> s -> FormFieldState s e Name
chunkSubsetField capacity stLens initialState = 
  let (initChunk, initInt) = initialState ^. stLens

      handleChunkEvent1 :: BrickEvent n e -> EventM n (Chunk, Int) ()
      handleChunkEvent1 (VtyEvent ev) = do
        s@(c@(Chunk i n), int) <- get
        case ev of
          V.EvKey (V.KChar c) [] | isDigit c -> do 
            let i' = read (show i ++ [c])
            put $ if i' <= n || n == 0 then (Chunk i' n, getSizeOfChunk (Chunk i' n)) else (Chunk n n, getSizeOfChunk (Chunk n n))
          V.EvKey V.KBS [] -> do
            let calcNew x = if null (show x) then 0 else fromMaybe 0 (readMaybe (init (show x)))
            put (Chunk (calcNew i) n, int)
          _ -> return ()
      handleChunkEvent1 _ = return ()

      handleChunkEvent2 :: BrickEvent n e -> EventM n (Chunk, Int) ()
      handleChunkEvent2 (VtyEvent ev) = do 
        s@(c@(Chunk i n), int) <- get
        case ev of
          V.EvKey (V.KChar c) [] | isDigit c -> do
            let n' = read (show n ++ [c])
                i' = if i <= n' || n' == 0 then i else n'
            put $ if n' <= capacity then (Chunk i' n', getSizeOfChunk (Chunk i' n')) else (Chunk i capacity, getSizeOfChunk (Chunk i capacity))
          V.EvKey V.KBS [] -> do
            let calcNew x = if null (show x) then 0 else fromMaybe 0 (readMaybe (init (show x)))
                newN = calcNew n
                newI = if i <= newN || newN == 0 then i else newN
            put (Chunk newI newN, int)
          _ -> return ()
      handleChunkEvent2 _ = return ()

      handleSubsetEvent :: BrickEvent n e -> EventM n (Chunk, Int) ()
      handleSubsetEvent (VtyEvent ev) = do
        s@(ch@(Chunk i n), int) <- get
        let bound = getSizeOfChunk ch in
          case ev of
          V.EvKey (V.KChar c) [] | isDigit c -> do
            let newValue = read (show int ++ [c])
                int' = min newValue bound
            put (ch, int')
          V.EvKey V.KBS [] -> do
            let int' = case show int of
                            "" -> 0
                            xs -> fromMaybe 0 (readMaybe (init xs))
            put (ch, int')
          _ -> return ()
      handleSubsetEvent _ = return ()

      renderChunk1 :: Bool -> (Chunk, Int) -> Widget Name
      renderChunk1 foc (Chunk i n, _) = 
        let addAttr = if foc then withDefAttr focusedFormInputAttr else id
            csr x = if foc then showCursor ChunkField1 (Location (length x,0)) else id
            val' 0 = ""
            val' x = show x
          in addAttr (csr (val' i) (str (val' i))) <+> str "/"

      renderChunk2 :: Bool -> (Chunk, Int) -> Widget Name
      renderChunk2 foc (Chunk i n, _) = 
        let addAttr = if foc then withDefAttr focusedFormInputAttr else id
            csr x = if foc then showCursor ChunkField2 (Location (length x,0)) else id
            val' 0 = ""
            val' x = show x
          in addAttr (csr (val' n) (str (val' n)))

      customConcat :: [Widget Name] -> Widget Name
      customConcat [chunk1, chunk2, subset] = 
        (str "Select chunk:" <+> hFill ' ' <+> chunk1 <+> chunk2) 
        <=>
        str " "
        <=>
        (str "Number of cards:" <+> hFill ' ' <+> subset)
        <=>
        str " "
      customConcat _ = error "chunkSubsetField form field concatenation has gone wrong"

      getSizeOfChunk :: Chunk -> Int
      getSizeOfChunk (Chunk i n) = 
        if i >= 1 && n >= 1 && i <= n
          then length (splitIntoNChunks n [1..capacity] !! (i-1))
          else capacity

      renderSubset :: Bool -> (Chunk, Int) -> Widget Name
      renderSubset foc (c, value) = 
        let cardsInChunk = getSizeOfChunk c
        in renderNaturalNumber cardsInChunk ("/" <> show cardsInChunk) SubsetField foc value
      
      validateChunk (c@(Chunk i n), int) = if i >= 1 && n >= 1 && i <= n then Just (c, int) else Nothing
      validateSubset = Just

  in FormFieldState { formFieldState = (initChunk, initInt)
                    , formFields = [ 
                                     FormField ChunkField1 validateChunk True
                                       renderChunk1
                                       handleChunkEvent1,
                                     FormField ChunkField2 validateChunk True
                                       renderChunk2
                                       handleChunkEvent2,
                                     FormField SubsetField validateSubset True
                                       renderSubset
                                       handleSubsetEvent
                                   ]
                    , formFieldLens = stLens
                    , formFieldUpdate = const
                    , formFieldRenderHelper = id
                    , formFieldConcat = customConcat
                    , formFieldVisibilityMode = ShowAugmentedField }

okField :: (Ord n, Show n) => Lens' s Bool -> n -> String -> s -> FormFieldState s e n
okField stLens name label initialState =
  let initVal = initialState ^. stLens

      handleEvent (VtyEvent (V.EvKey V.KEnter [])) = put True
      handleEvent _ = return ()
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderOk label name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldUpdate = const
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox
                    , formFieldVisibilityMode = ShowAugmentedField }

renderOk :: String -> n -> Bool -> Bool -> Widget n
renderOk label _ focus _ =
  (if focus then withAttr selectedAttr else id) $ str "Ok"
