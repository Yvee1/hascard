{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Parameters where
import UI.Attributes
import Brick
import Brick.Widgets.Center
import Brick.Forms
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
mkParameterForm n =
  let label s w = padBottom (Pad 1) $ padRight (Pad 2) (strWrap s) <+> w
  in newForm
    [ label "Shuffle the deck?" @@= yesnoField True pShuffle ShuffleField ""
    , label "Number of cards:" @@= naturalNumberField n (subsetLens n) SubsetField ("/" <> pack (show n))
    , label "Select chunk:" @@= chunkField n pChunk ChunkField
    , label "Review mode?" @@= yesnoField True pReviewMode ReviewModeField ""
    , hCenter @@= okField pOk ParametersOkField "Ok"
    ]

subsetLens :: Int -> Lens' Parameters Int
subsetLens n f ps =
  (\n -> ps & pSubset ?~ n)
  <$> f (fromMaybe n (ps ^. pSubset))

chunkField :: (Ord n, Show n) => Int -> Lens' s Chunk -> n -> s -> FormFieldState s e n
chunkField bound stLens name initialState =
  let initVal = initialState ^. stLens

      handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) (Chunk i n, p) | isDigit c =
        if p 
          then let newValue = read (show n ++ [c])
            in return $ if newValue <= bound then (Chunk i newValue, p) else (Chunk i bound, p)
          else let newValue = read (show i ++ [c])
            in return $ if newValue <= n     then (Chunk newValue n, p) else (Chunk n n, p)
      handleEvent (VtyEvent (V.EvKey V.KBS [])) (Chunk i n, p) = 
        let calcNew x = if null (show x) then 0 else fromMaybe 0 (readMaybe (init (show x)))
        in if p
          then return $
            let newN = calcNew n
                newI = if i <= newN then i else newN
            in (Chunk newI newN, p)
          else return (Chunk (calcNew i) n, p)
      handleEvent (VtyEvent (V.EvKey V.KRight [])) (c, _) = return (c, True)
      handleEvent (VtyEvent (V.EvKey V.KLeft  [])) (c, _) = return (c, False)
      handleEvent (VtyEvent (V.EvKey V.KEnter [])) (c, p) = return (c, not p)
      handleEvent _ s = return s

      validate (c@(Chunk i n), _) = if i >= 1 && n >= 1 && i <= n then Just c else Nothing
  
  in FormFieldState { formFieldState = (initVal, False)
                    , formFields = [ FormField name validate True 
                                       (renderChunk bound name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderChunk :: Int -> n -> Bool -> (Chunk, Bool) -> Widget n
renderChunk bound name foc (Chunk i n, p) =
  let addAttr = if foc then withDefAttr focusedFormInputAttr else id
      csr x = if foc then showCursor name (Location (length x,0)) else id
      val' 0 = ""
      val' x = show x
    in if p
      then str (val' i) <+> str "/" <+> addAttr (csr (val' n) (str (val' n)))
      else addAttr (csr (val' i) (str (val' i))) <+> str "/" <+> str (val' n)

okField :: (Ord n, Show n) => Lens' s Bool -> n -> T.Text -> s -> FormFieldState s e n
okField stLens name label initialState =
  let initVal = initialState ^. stLens

      handleEvent (VtyEvent (V.EvKey V.KEnter [])) _  = return True
      handleEvent _ s = return s
  
  in FormFieldState { formFieldState = initVal
                    , formFields = [ FormField name Just True 
                                       (renderOk label name)
                                       handleEvent ]
                    , formFieldLens = stLens
                    , formFieldRenderHelper = id
                    , formFieldConcat = vBox }

renderOk :: T.Text -> n -> Bool -> Bool -> Widget n
renderOk label _ focus _ =
  (if focus then withAttr selectedAttr else id) $ str "Ok"
