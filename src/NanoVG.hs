module NanoVG
  (module NanoVG.Internal
  ,textBreakLines
  ) where

import           NanoVG.Internal hiding (textBreakLines)

import           Control.Monad
import qualified Data.Text as T
import           Data.Text.Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified NanoVG.Internal as Internal

-- | High level abstraction of NanoVG.Internal.textBreakLines
-- This uses the fonts for width calculations so make sure you have them setup properly
textBreakLines :: Context -> T.Text -> CFloat -> CInt -> (TextRow -> CInt -> IO ()) -> IO ()
textBreakLines c text' width' chunkSize f =
  withCStringLen text' $
  \(startPtr,len) ->
    allocaBytesAligned (sizeOf (undefined :: TextRow) * fromIntegral chunkSize)
                       (alignment (undefined :: TextRow)) $
    \arrayPtr ->
      do let endPtr = startPtr `plusPtr` len
             loop line ptr =
               do count <-
                    Internal.textBreakLines c ptr endPtr width' arrayPtr 3
                  when (count > 0) $
                    loop (line + count) =<< readChunk line arrayPtr count
         loop 0 startPtr
  where readChunk
          :: CInt -> TextRowPtr -> CInt -> IO (Ptr CChar)
        readChunk baseline arrayPtr count =
          do forM_ [0 .. (count - 1)] $
               \i ->
                 do textRow <-
                      peekElemOff arrayPtr
                                  (fromIntegral i)
                    f textRow (baseline + i)
             next <$>
               peekElemOff arrayPtr
                           (fromIntegral (count - 1))
