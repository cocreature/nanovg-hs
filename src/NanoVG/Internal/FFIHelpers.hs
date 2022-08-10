module NanoVG.Internal.FFIHelpers
  (withCString
  ,useAsCStringLen'
  ,useAsPtr
  ,one
  ,null
  ,bitMask
  ) where

import           Control.Monad ((>=>))
import           Data.Bits ((.|.))
import           Data.ByteString hiding (null)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Marshal (copyBytes, mallocBytes)
import           Foreign.Ptr
import           Prelude hiding (null)

-- | Marshal a Haskell string into a NUL terminated C string using temporary storage.
withCString :: T.Text -> (CString -> IO b) -> IO b
withCString t = useAsCString (T.encodeUtf8 t)

-- | Wrapper around 'useAsCStringLen' that uses 'CUChar's
useAsCStringLen' :: ByteString -> ((Ptr CUChar,CInt) -> IO a) -> IO a
useAsCStringLen' bs f = useAsCStringLen bs ((\(ptr,len) -> return (castPtr ptr,fromIntegral len)) >=> copyCStringLen >=> f)
  where
    -- | Copy memory under given pointer to a new address.
    -- The allocated memory is not garbage-collected and needs to be freed manually later.
    -- In the case of 'createFontMem' and 'createFontMemAtIndex' (the only places using it)
    -- it is freed by NanoVG as a part of 'nvgDeleteGL3'.
    copyCStringLen :: Integral b => (Ptr a, b) -> IO (Ptr a, b)
    copyCStringLen (from, len) =
      let intLen = fromIntegral len
      in do
        to <- mallocBytes intLen
        copyBytes to from intLen
        return (to, len)

-- | Wrapper around 'useAsCStringLen'' that discards the length
useAsPtr :: ByteString -> (Ptr CUChar -> IO a) -> IO a
useAsPtr bs f = useAsCStringLen' bs (f . fst)

-- | Marshalling helper for a constant one
one :: Num a => (a -> b) -> b
one f = f 1

-- | Marshalling helper for a constant 'nullPtr'
null :: (Ptr a -> b) -> b
null f = f nullPtr

-- | Combine the values in the set using a bitwise or
bitMask :: Enum a => S.Set a -> CInt
bitMask = S.fold (.|.) 0 . S.map (fromIntegral . fromEnum)

