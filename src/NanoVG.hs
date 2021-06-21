{-# LANGUAGE CPP #-}
module NanoVG
  ( FileName(..)
  , Context(..)
  , Extent(..)
  , Solidity(..)
  , LineCap(..)
  , beginFrame
  , cancelFrame
  , endFrame
  -- * Color utils
  , Color(..)
  , rgb
  , rgbf
  , rgba
  , rgbaf
  , lerpRGBA
  , transRGBA
  , transRGBAf
  , hsl
  , hsla
  -- * State handling
  , save
  , restore
  , reset
  -- * Render styles
  , strokeColor
  , strokePaint
  , fillColor
  , fillPaint
  , miterLimit
  , strokeWidth
  , lineCap
  , lineJoin
  , globalAlpha
  -- * Transforms
  , Transformation(..)
  , resetTransform
  , transform
  , translate
  , rotate
  , skewX
  , skewY
  , scale
  , currentTransform
  , transformIdentity
  , transformTranslate
  , transformScale
  , transformRotate
  , transformSkewX
  , transformSkewY
  , transformMultiply
  , transformPremultiply
  , transformInverse
  , transformPoint
  , degToRad
  , radToDeg
  -- * Images
  , Image(..)
  , createImage
  , createImageMem
  , createImageRGBA
  , updateImage
  , imageSize
  , deleteImage
  -- * Paints
  , Paint(..)
  , linearGradient
  , boxGradient
  , radialGradient
  , imagePattern
  -- * Scissoring
  , scissor
  , intersectScissor
  , resetScissor
  -- * Paths
  , beginPath
  , moveTo
  , lineTo
  , bezierTo
  , quadTo
  , arcTo
  , closePath
  , Winding(..)
  , pathWinding
  , arc
  , rect
  , roundedRect
  , roundedRectVarying
  , ellipse
  , circle
  , fill
  , stroke
  -- * Global Composite
  , BlendFactor(..)
  , CompositeOperation(..)
  , globalCompositeOperation
  , globalCompositeBlendFunc
  , globalCompositeBlendFuncSeparate
  -- * Text
  , Font(..)
  , createFont
  , createFontAtIndex
  , createFontMem
  , createFontMemAtIndex
  , findFont
  , addFallbackFontId
  , addFallbackFont
  , resetFallbackFontsId
  , resetFallbackFonts
  , fontSize
  , fontBlur
  , textLetterSpacing
  , textLineHeight
  , Align(..)
  , textAlign
  , fontFaceId
  , fontFace
  , text
  , textBox
  , Bounds(..)
  , textBounds
  , textBoxBounds
  , GlyphPosition(..)
  , GlyphPositionPtr
  , textGlyphPositions
  , textMetrics
  , TextRow(..)
  , TextRowPtr
  , textBreakLines
  -- * GL
  , CreateFlags(..)
#if defined(GLES_3)
  , createGLES3
  , deleteGLES3
  , createImageFromHandleGLES3
  , imageHandleGLES3
#elif defined(GL_2)
  , createGL2
  , deleteGL2
  , createImageFromHandleGL2
  , imageHandleGL2
#else
  , createGL3
  , deleteGL3
  , createImageFromHandleGL3
  , imageHandleGL3
#endif
  -- * Vector types
  , V2(..)
  , V3(..)
  , V4(..)
  , M23
  ) where

import           Data.Functor ((<$>))
import           Control.Monad
import qualified Data.Text as T
import           Data.Text.Foreign
import qualified Data.Vector as V
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           NanoVG.Internal
import           NanoVG.Internal.CreateContext
#if defined(GLES_3)
import           NanoVG.Internal.GLES3
#elif defined(GL_2)
import           NanoVG.Internal.GL2
#else
import           NanoVG.Internal.GL3
#endif
import qualified NanoVG.Internal.Text as Internal
import           NanoVG.Internal.Text hiding (textBreakLines,textGlyphPositions,text)

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
