{-# LANGUAGE TemplateHaskell #-}
module Export where

import Lens.Micro.Platform
import qualified Data.List.NonEmpty as NE
import Types
import Data.Either.Extra (mapRight)
import Data.List.Extra (replace)
import Data.List (intercalate, stripPrefix, tails, isPrefixOf)

data ExportOpts = ExportOpts
  { _optExportInput       :: String 
  , _optExportOutput      :: String
  , _optCardDelimiter     :: String 
  , _optQuestionDelimiter :: String 
  }

makeLenses ''ExportOpts

exportCards :: ExportOpts -> [Card] -> Either String String
exportCards opts = mapRight (intercalate (opts ^. optCardDelimiter)) 
                 . mapM (exportCard opts)

exportCard :: ExportOpts -> Card -> Either String String
exportCard opts (Definition {question=q, definition=d}) = Right $
  escape opts q <> (opts ^. optQuestionDelimiter) <> escape opts d
exportCard opts (OpenQuestion {question=q, perforated=p}) = Right $
  escape opts q <> (opts ^. optQuestionDelimiter) <> escape opts (fillPerforated p)
exportCard _ _ = Left "Only definition and open question cards can be exported."

fillPerforated :: Perforated -> String
fillPerforated = foldSentence id fSent . perforatedToSentence
  where fSent pre gaps post = pre <> concat (NE.intersperse "/" gaps) <> post

escape :: ExportOpts -> String -> String
escape opts s = if s `contains` (opts ^. optCardDelimiter) || s `contains` (opts ^. optQuestionDelimiter)
  then "\"" <> replace "\"" "\"\"" s <> "\""
  else s

contains :: Eq a => [a] -> [a] -> Bool
contains str substr = any (isPrefixOf substr) (tails str)
