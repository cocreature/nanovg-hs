module NanoVG.Internal.FFIHelpers
  (withCString
  ,useAsCStringLen'
  ,useAsPtr
  ,zero
  ,null
  ,bitMask
  ) where

import           Data.Bits ((.|.))
import           Data.ByteString hiding (null)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Ptr
import           Prelude hiding (null)

-- | Marshal a Haskell string into a NUL terminated C string using temporary storage.
withCString :: T.Text -> (CString -> IO b) -> IO b
withCString t = useAsCString (T.encodeUtf8 t)

-- | Wrapper around 'useAsCStringLen' that uses 'CUChar's
useAsCStringLen' :: ByteString -> ((Ptr CUChar,CInt) -> IO a) -> IO a
useAsCStringLen' bs f = useAsCStringLen bs (\(ptr,len) -> f (castPtr ptr,fromIntegral len))

-- | Wrapper around 'useAsCStringLen'' that discards the length
useAsPtr :: ByteString -> (Ptr CUChar -> IO a) -> IO a
useAsPtr bs f = useAsCStringLen' bs (f . fst)

-- | Marshalling helper for a constant zero
zero :: Num a => (a -> b) -> b
zero f = f 0

-- | Marshalling helper for a constant 'nullPtr'
null :: (Ptr a -> b) -> b
null f = f nullPtr

-- | Combine the values in the set using a bitwise or
bitMask :: Enum a => S.Set a -> CInt
bitMask = S.fold (.|.) 0 . S.map (fromIntegral . fromEnum)

