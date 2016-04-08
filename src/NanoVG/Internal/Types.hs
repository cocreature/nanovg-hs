module NanoVG.Internal.Types 
  (FileName(..)
  ,Image(..)) where

import qualified Data.Text as T
import           Foreign.C.Types

-- | Newtype to avoid accidental use of strings
newtype FileName = FileName { unwrapFileName :: T.Text }

-- | Newtype to avoid accidental use of ints
newtype Image = Image {imageHandle :: CInt} deriving (Show,Read,Eq,Ord)
