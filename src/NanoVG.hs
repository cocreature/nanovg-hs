module NanoVG
  (module NanoVG.Internal
  ,textBreakLines
  ,textGlyphPositions
  ,text
  ) where

import qualified Data.Vector as V
import           Control.Monad
import qualified Data.Text as T
import           Data.Text.Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified NanoVG.Internal as Internal
import           NanoVG.Internal hiding (textBreakLines,textGlyphPositions,text)

-- | High level wrapper around NanoVG.Internal.textBreakLines
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
                    Internal.textBreakLines c ptr endPtr width' arrayPtr chunkSize
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

-- | High level wrapper around NanoVG.Internal.textGlyphPositions
-- Might be changed to return a vector in the future
textGlyphPositions :: Context -> CFloat -> CFloat -> Ptr CChar -> Ptr CChar -> CInt -> IO (V.Vector GlyphPosition)
textGlyphPositions c x y startPtr endPtr maxGlyphs =
  allocaBytesAligned
    (sizeOf (undefined :: GlyphPosition) * fromIntegral maxGlyphs)
    (alignment (undefined :: GlyphPosition)) $
  \arrayPtr ->
    do count <-
         Internal.textGlyphPositions c x y startPtr endPtr arrayPtr maxGlyphs
       readChunk arrayPtr count
  where readChunk
          :: GlyphPositionPtr -> CInt -> IO (V.Vector GlyphPosition)
        readChunk arrayPtr count =
          V.generateM (fromIntegral count) $
          \i ->
            peekElemOff arrayPtr
                        (fromIntegral i)

text :: Context -> CFloat -> CFloat -> T.Text -> IO ()
text c x y t = withCStringLen t $ \(ptr,len) -> Internal.text c x y ptr (ptr `plusPtr` len)
