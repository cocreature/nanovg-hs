module NanoVG.Internal
  ( FileName(..)
  , Image(..)
  , Font(..)
  , Context(..)
  , Transformation(..)
  , Extent(..)
  , Color(..)
  , Paint(..)
  , Bounds(..)
  , GlyphPosition(..)
  , GlyphPositionPtr
  , TextRow(..)
  , TextRowPtr
  , CreateFlags(..)
  , Solidity(..)
  , LineCap(..)
  , Align(..)
  , Winding(..)
  , beginFrame
  , cancelFrame
  , endFrame
  -- * Color utils
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
  , createImage
  , createImageMem
  , createImageRGBA
  , updateImage
  , imageSize
  , deleteImage
  -- * Paints
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
  , pathWinding
  , arc
  , rect
  , roundedRect
  , ellipse
  , circle
  , fill
  , stroke
  -- * Text
  , createFont
  , createFontMem
  , findFont
  , fontSize
  , fontBlur
  , textLetterSpacing
  , textLineHeight
  , textAlign
  , fontFaceId
  , fontFace
  , text
  , textBox
  , textBounds
  , textBoxBounds
  , textGlyphPositions
  , textMetrics
  , textBreakLines
  -- * GL
  , createGL3
  , deleteGL3
  , createImageFromHandleGL3
  , imageHandleGL3
  ) where

import           Foreign.C.Types

import           NanoVG.Internal.Color
import           NanoVG.Internal.Context
import           NanoVG.Internal.Paint
import           NanoVG.Internal.Path
import           NanoVG.Internal.Scissor
import           NanoVG.Internal.Image
import           NanoVG.Internal.State
import           NanoVG.Internal.Style
import           NanoVG.Internal.Text
import           NanoVG.Internal.Transformation
import           NanoVG.Internal.Types
import           NanoVG.Internal.GL3

{#pointer *NVGcontext as Context newtype nocode#}

#include "nanovg.h"

{#enum NVGsolidity as Solidity
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

-- | Begin drawing a new frame
--
-- Calls to nanovg drawing API should be wrapped in 'beginFrame' & 'endFrame'.
--
-- 'beginFrame' defines the size of the window to render to in relation currently
-- set viewport (i.e. glViewport on GL backends). Device pixel ration allows to
-- control the rendering on Hi-DPI devices.
--
-- For example, GLFW returns two dimension for an opened window: window size and
-- frame buffer size. In that case you would set windowWidth/Height to the window size
-- devicePixelRatio to: frameBufferWidth / windowWidth.
{#fun unsafe nvgBeginFrame as beginFrame
        {`Context',`CInt',`CInt',`Float'} -> `()'#}

-- | Cancels drawing the current frame.
{#fun unsafe nvgCancelFrame as cancelFrame
        {`Context'} -> `()'#}

-- | Ends drawing flushing remaining render state.
{#fun unsafe nvgEndFrame as endFrame
        {`Context'} -> `()'#}
