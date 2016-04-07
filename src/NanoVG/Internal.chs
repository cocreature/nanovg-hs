{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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

import           Data.Bits hiding (rotate)
import           Data.ByteString hiding (null)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Prelude hiding (null)

-- For now only the GL3 backend is supported
#define NANOVG_GL3
-- We need to include this to define GLuint
#include "GL/glew.h"
#include "nanovg.h"
#include "nanovg_gl.h"
#include "nanovg_wrapper.h"

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

-- | Newtype to avoid accidental use of strings
newtype FileName = FileName { unwrapFileName :: T.Text }

-- | Newtype to avoid accidental use of ints
newtype Image = Image {imageHandle :: CInt} deriving (Show,Read,Eq,Ord)

-- | Newtype to avoid accidental use of ints
newtype Font = Font {fontHandle :: CInt} deriving (Show,Read,Eq,Ord)

-- | Opaque context that needs to be passed around
{#pointer *NVGcontext as Context newtype#}

-- | Affine matrix
--
-- > [sx kx tx]
-- > [ky sy ty]
-- > [ 0  0  1]
newtype Transformation = Transformation (M23 CFloat) deriving (Show,Read,Eq,Ord)

instance Storable Transformation where
  sizeOf _ = sizeOf (0 :: CFloat) * 6
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       a <- peek p'
       b <- peekElemOff p' 1
       c <- peekElemOff p' 2
       d <- peekElemOff p' 3
       e <- peekElemOff p' 4
       f <- peekElemOff p' 5
       pure (Transformation
               (V2 (V3 a c e)
                   (V3 b d f)))
  poke p (Transformation (V2 (V3 a c e) (V3 b d f))) =
    do let p' = castPtr p :: Ptr CFloat
       poke p' a
       pokeElemOff p' 1 b
       pokeElemOff p' 2 c
       pokeElemOff p' 3 d
       pokeElemOff p' 4 e
       pokeElemOff p' 5 f

newtype Extent = Extent (V2 CFloat) deriving (Show,Read,Eq,Ord)

instance Storable Extent where
  sizeOf _ = sizeOf (0 :: CFloat) * 2
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       a <- peekElemOff p' 0
       b <- peekElemOff p' 1
       pure (Extent (V2 a b))
  poke p (Extent (V2 a b)) =
    do let p' = castPtr p :: Ptr CFloat
       pokeElemOff p' 0 a
       pokeElemOff p' 1 b

-- | rgba
data Color = Color !CFloat !CFloat !CFloat !CFloat deriving (Show,Read,Eq,Ord)

{#pointer *NVGcolor as ColorPtr -> Color#}

instance Storable Color where
  sizeOf _ = sizeOf (0 :: CFloat) * 4
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       r <- peek p'
       g <- peekElemOff p' 1
       b <- peekElemOff p' 2
       a <- peekElemOff p' 3
       pure (Color r g b a)
  poke p (Color r g b a) =
    do let p' = castPtr p :: Ptr CFloat
       poke p' r
       pokeElemOff p' 1 g
       pokeElemOff p' 2 b
       pokeElemOff p' 3 a

data Paint =
  Paint {xform :: Transformation
        ,extent :: Extent
        ,radius :: !CFloat
        ,feather :: !CFloat
        ,innerColor :: !Color
        ,outerColor :: !Color
        ,image :: !Image} deriving (Show,Read,Eq,Ord)

{#pointer *NVGpaint as PaintPtr -> Paint#}

instance Storable Paint where
  sizeOf _ = 76
  alignment _ = 4
  peek p =
    do xform <- peek (castPtr (p `plusPtr` {#offsetof NVGpaint->xform#}))
       extent <- peek (castPtr (p `plusPtr` {#offsetof NVGpaint->extent#}))
       radius <- {#get NVGpaint->radius#} p
       feather <- {#get NVGpaint->feather#} p
       innerColor <- peek (castPtr (p `plusPtr` 40))
       outerColor <- peek (castPtr (p `plusPtr` 56))
       image <- peek (castPtr (p `plusPtr` 72))
       pure (Paint xform extent radius feather innerColor outerColor (Image image))
  poke p (Paint{..}) =
    do poke (castPtr (p `plusPtr` {#offsetof NVGpaint->xform#})) xform
       poke (castPtr (p `plusPtr` {#offsetof NVGpaint->extent#})) extent
       {#set NVGpaint->radius#} p radius
       {#set NVGpaint->feather#} p feather
       poke (castPtr (p `plusPtr` 40)) innerColor
       poke (castPtr (p `plusPtr` 56)) outerColor
       poke (castPtr (p `plusPtr` 72)) (imageHandle image)

{#enum NVGwinding as Winding
         {} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

{#enum NVGsolidity as Solidity
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

{#enum NVGlineCap as LineCap
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

{#enum NVGalign as Align 
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

data GlyphPosition =
     GlyphPosition { -- | Pointer of the glyph in the input string.
                     str :: !(Ptr CChar)
                     -- | The x-coordinate of the logical glyph position.
                   , glyphX :: !CFloat
                     -- | The left bound of the glyph shape.
                   , glyphPosMinX :: !CFloat
                     -- | The right bound of the glyph shape.
                   , glyphPosMaxX :: !CFloat} deriving (Show,Eq,Ord)

{#pointer *NVGglyphPosition as GlyphPositionPtr -> GlyphPosition#}

instance Storable GlyphPosition where
  sizeOf _ = 24
  alignment _ = {#alignof NVGglyphPosition#}
  peek p =
    do str <- {#get NVGglyphPosition->str#} p
       x <- {#get NVGglyphPosition->x#} p
       minx <- {#get NVGglyphPosition->minx#} p
       maxx <- {#get NVGglyphPosition->maxx#} p
       pure (GlyphPosition str x minx maxx)
  poke p (GlyphPosition str x minx maxx) =
    do {#set NVGglyphPosition->str#} p str
       {#set NVGglyphPosition->x#} p x
       {#set NVGglyphPosition->minx#} p minx
       {#set NVGglyphPosition->maxx#} p maxx

data TextRow =
  TextRow { -- | Pointer to the input text where the row starts.
            start :: !(Ptr CChar)
            -- | Pointer to the input text where the row ends (one past the last character).
          , end :: !(Ptr CChar)
            -- | Pointer to the beginning of the next row.
          , next :: !(Ptr CChar)
            -- | Logical width of the row.
          , width :: !CFloat
            -- | Actual bounds of the row. Logical with and bounds can differ because of kerning and some parts over extending.
          , textRowMinX :: !CFloat
          , textRowMaxX :: !CFloat}
  deriving (Show,Eq,Ord)

instance Storable TextRow where
  sizeOf _ = 40
  alignment _ = {#alignof NVGtextRow#}
  peek p =
    do start <- {#get NVGtextRow->start#} p
       end <- {#get NVGtextRow->end#} p
       next <- {#get NVGtextRow->next#} p
       width <- {#get NVGtextRow->width#} p
       minX <- {#get NVGtextRow->minx#} p
       maxX <- {#get NVGtextRow->maxx#} p
       pure (TextRow start end next width minX maxX)
  poke p (TextRow {..}) =
    do {#set NVGtextRow->start#} p start
       {#set NVGtextRow->end#} p end
       {#set NVGtextRow->next#} p next
       {#set NVGtextRow->width#} p width
       {#set NVGtextRow->minx#} p textRowMinX
       {#set NVGtextRow->maxx#} p textRowMaxX

{#pointer *NVGtextRow as TextRowPtr -> TextRow#}

{#enum NVGimageFlags as ImageFlags
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

-- | Returns a color value from red, green, blue values. Alpha will be set to 255 (1.0f).
{#fun pure unsafe nvgRGB_ as rgb
        {id`CUChar',id`CUChar',id`CUChar',alloca-`Color'peek*} -> `()'#}

-- | Returns a color value from red, green, blue values. Alpha will be set to 1.0f.
{#fun pure unsafe nvgRGBf_ as rgbf
        {`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

-- | Returns a color value from red, green, blue and alpha values.
{#fun pure unsafe nvgRGBA_ as rgba
        {id`CUChar',id`CUChar',id`CUChar',id`CUChar',alloca-`Color'peek*} -> `()'#}

-- | Returns a color value from red, green, blue and alpha values.
{#fun pure unsafe nvgRGBAf_ as rgbaf
        {`CFloat',`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

-- | Linearly interpolates from color c0 to c1, and returns resulting color value.
{#fun pure unsafe nvgLerpRGBA_ as lerpRGBA
        {with*`Color',with*`Color',`CFloat',alloca-`Color'peek*} -> `()'#}

-- | Sets transparency of a color value.
{#fun pure unsafe nvgTransRGBA_ as transRGBA
        {with*`Color',id`CUChar',alloca-`Color'peek*} -> `()'#}

-- | Sets transparency of a color value.
{#fun pure unsafe nvgTransRGBAf_ as transRGBAf
        {with*`Color',`CFloat',alloca-`Color'peek*} -> `()'#}

-- | Returns color value specified by hue, saturation and lightness.
-- HSL values are all in range [0..1], alpha will be set to 255.
{#fun pure unsafe nvgHSL_ as hsl
        {`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

-- | Returns color value specified by hue, saturation and lightness and alpha.
-- HSL values are all in range [0..1], alpha in range [0..255]
{#fun pure unsafe nvgHSLA_ as hsla
        {`CFloat',`CFloat',`CFloat',id`CUChar',alloca-`Color'peek*} -> `()'#}

-- | Pushes and saves the current render state into a state stack.
-- A matching 'restore' must be used to restore the state.

{#fun unsafe nvgSave as save
        {`Context'} -> `()'#}

-- | Pops and restores current render state.
{#fun unsafe nvgRestore as restore
        {`Context'} -> `()'#}

-- | Resets current render state to default values. Does not affect the render state stack.
{#fun unsafe nvgReset as reset
        {`Context'} -> `()'#}

-- | Sets current stroke style to a solid color.
{#fun unsafe nvgStrokeColor_ as strokeColor
        {`Context',with*`Color'} -> `()'#}

-- | Sets current stroke style to a paint, which can be a one of the gradients or a pattern.
{#fun unsafe nvgStrokePaint_ as strokePaint
        {`Context',with*`Paint'} -> `()'#}

-- | Sets current fill style to a solid color.
{#fun unsafe nvgFillColor_ as fillColor
        {`Context',with*`Color'} -> `()'#}

-- | Sets current fill style to a paint, which can be a one of the gradients or a pattern.
{#fun unsafe nvgFillPaint_ as fillPaint
        {`Context',with*`Paint'} -> `()'#}

-- | Sets the miter limit of the stroke style.
-- Miter limit controls when a sharp corner is beveled.
{#fun unsafe nvgMiterLimit as miterLimit
        {`Context',`CFloat'} -> `()'#}

-- | Sets the stroke width of the stroke style.
{#fun unsafe nvgStrokeWidth as strokeWidth
        {`Context',`CFloat'} -> `()'#}

-- | Sets how the end of the line (cap) is drawn,
-- Can be one of: 'Butt' (default), 'Round', 'Square'.
{#fun unsafe nvgLineCap as lineCap
        {`Context',`LineCap'} -> `()'#}

-- | Sets how sharp path corners are drawn.
-- Can be one of 'Miter' (default), 'Round', 'Bevel.
{#fun unsafe nvgLineJoin as lineJoin
        {`Context',`LineCap'} -> `()'#}

-- | Sets the transparency applied to all rendered shapes.
-- Already transparent paths will get proportionally more transparent as well.
{#fun unsafe nvgGlobalAlpha as globalAlpha
        {`Context',`CFloat'} -> `()'#}

-- | Resets current transform to a identity matrix.
{#fun unsafe nvgResetTransform as resetTransform
        {`Context'} -> `()'#}

-- | Premultiplies current coordinate system by specified matrix.
-- The parameters are interpreted as matrix as follows:
--
-- > [a c e]
-- > [b d f]
-- > [0 0 1]
{#fun unsafe nvgTransform as transform
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Translates current coordinate system.
{#fun unsafe nvgTranslate as translate
        {`Context',`CFloat',`CFloat'} -> `()'#}

-- | Rotates current coordinate system. Angle is specified in radians.
{#fun unsafe nvgRotate as rotate
        {`Context',`CFloat'} -> `()'#}

-- | Skews the current coordinate system along X axis. Angle is specified in radians.
{#fun unsafe nvgSkewX as skewX
        {`Context',`CFloat'} -> `()'#}

-- | Skews the current coordinate system along Y axis. Angle is specified in radians.
{#fun unsafe nvgSkewY as skewY
        {`Context',`CFloat'} -> `()'#}

-- | Scales the current coordinate system.
{#fun unsafe nvgScale as scale
        {`Context',`CFloat',`CFloat'} -> `()'#}

peekTransformation :: Ptr CFloat -> IO Transformation
peekTransformation = peek . castPtr

allocaTransformation :: (Ptr CFloat -> IO b) -> IO b
allocaTransformation f = alloca (\(p :: Ptr Transformation) -> f (castPtr p))

withTransformation :: Transformation -> (Ptr CFloat -> IO b) -> IO b
withTransformation t f = with t (\p -> f (castPtr p)) 

-- | Returns the current transformation matrix.
{#fun unsafe nvgCurrentTransform as currentTransform
        {`Context',allocaTransformation-`Transformation'peekTransformation*} -> `()'#}

-- | Sets the transform to identity matrix.
{#fun unsafe nvgTransformIdentity as transformIdentity
        {allocaTransformation-`Transformation'peekTransformation*} -> `()'#}

-- | Sets the transform to translation matrix matrix.
{#fun unsafe nvgTransformTranslate as transformTranslate
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat',`CFloat'} -> `()'#}

-- | Sets the transform to scale matrix.
{#fun unsafe nvgTransformScale as transformScale
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat',`CFloat'} -> `()'#}

-- | Sets the transform to rotate matrix. Angle is specified in radians.
{#fun unsafe nvgTransformRotate as transformRotate
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

-- | Sets the transform to skew-x matrix. Angle is specified in radians.
{#fun unsafe nvgTransformSkewX as transformSkewX
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

-- | Sets the transform to skew-y matrix. Angle is specified in radians.
{#fun unsafe nvgTransformSkewY as transformSkewY
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

-- | Sets the transform to the result of multiplication of two transforms, of A = A*B.
{#fun unsafe nvgTransformMultiply as transformMultiply
        {withTransformation*`Transformation'peekTransformation*,withTransformation*`Transformation'} -> `()'#}

-- | Sets the transform to the result of multiplication of two transforms, of A = B*A.
{#fun unsafe nvgTransformPremultiply as transformPremultiply
        {withTransformation*`Transformation'peekTransformation*,withTransformation*`Transformation'} -> `()'#}

-- | Sets the destination to inverse of specified transform.
-- Returns 1 if the inverse could be calculated, else 0.
{#fun unsafe nvgTransformInverse as transformInverse
        {allocaTransformation-`Transformation'peekTransformation*,withTransformation*`Transformation'} -> `()'#}

-- | Transform a point by given transform.
{#fun pure unsafe nvgTransformPoint as transformPoint
        {alloca-`CFloat'peek*,alloca-`CFloat'peek*,withTransformation*`Transformation',`CFloat',`CFloat'} -> `()'#}

-- | Converts degrees to radians.
{#fun pure unsafe nvgDegToRad as degToRad
        {`CFloat'} -> `CFloat'#}

-- | Converts radians to degrees.
{#fun pure unsafe nvgRadToDeg as radToDeg
        {`CFloat'} -> `CFloat'#}

safeImage :: CInt -> Maybe Image
safeImage i
  | i < 0 = Nothing
  | otherwise = Just (Image i)

-- | Creates image by loading it from the disk from specified file name.
{#fun unsafe nvgCreateImage as createImage
        {`Context','withCString.unwrapFileName'*`FileName',`CInt'} -> `Maybe Image'safeImage#}

-- | Creates image by loading it from the specified chunk of memory.
{#fun unsafe nvgCreateImageMem as createImageMem
        {`Context',`ImageFlags',useAsCStringLen'*`ByteString'&} -> `Maybe Image'safeImage#}

-- | Creates image from specified image data.
{#fun unsafe nvgCreateImageRGBA as createImageRGBA
        {`Context',`CInt',`CInt',`ImageFlags',useAsPtr*`ByteString'} -> `Maybe Image'safeImage#}

-- | Updates image data specified by image handle.
{#fun unsafe nvgUpdateImage as updateImage
        {`Context',imageHandle`Image',useAsPtr*`ByteString'} -> `()'#}

-- | Returns the dimensions of a created image.
{#fun unsafe nvgImageSize as imageSize
        {`Context',imageHandle`Image',alloca-`CInt'peek*,alloca-`CInt'peek*} -> `()'#}

-- | Deletes created image.
{#fun unsafe nvgDeleteImage as deleteImage
        {`Context',imageHandle`Image'} -> `()'#}

-- | Creates and returns a linear gradient. Parameters (sx,sy)-(ex,ey) specify the start and end coordinates
-- of the linear gradient, icol specifies the start color and ocol the end color.
-- The gradient is transformed by the current transform when it is passed to 'fillPaint' or 'strokePaint'.
{#fun unsafe nvgLinearGradient_ as linearGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

-- | Creates and returns a box gradient. Box gradient is a feathered rounded rectangle, it is useful for rendering
-- drop shadows or highlights for boxes. Parameters (x,y) define the top-left corner of the rectangle,
-- (w,h) define the size of the rectangle, r defines the corner radius, and f feather. Feather defines how blurry
-- the border of the rectangle is. Parameter icol specifies the inner color and ocol the outer color of the gradient.
-- The gradient is transformed by the current transform when it is passed to 'fillPaint' or 'strokePaint'.
{#fun unsafe nvgBoxGradient_ as boxGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

-- | Creates and returns a radial gradient. Parameters (cx,cy) specify the center, inr and outr specify
-- the inner and outer radius of the gradient, icol specifies the start color and ocol the end color.
-- The gradient is transformed by the current transform when it is passed to 'fillPaint' or 'strokePaint'.
{#fun unsafe nvgRadialGradient_ as radialGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

-- | Creates and returns an image patter. Parameters (ox,oy) specify the left-top location of the image pattern,
-- (ex,ey) the size of one image, angle rotation around the top-left corner, image is handle to the image to render.
-- The gradient is transformed by the current transform when it is passed to 'fillPaint' or 'strokePaint'.
{#fun unsafe nvgImagePattern_ as imagePattern
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',imageHandle`Image',`CFloat',alloca-`Paint'peek*} -> `()'#}

-- | Sets the current scissor rectangle.
-- The scissor rectangle is transformed by the current transform.
{#fun unsafe nvgScissor as scissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Intersects current scissor rectangle with the specified rectangle.
-- The scissor rectangle is transformed by the current transform.
-- Note: in case the rotation of previous scissor rect differs from
-- the current one, the intersection will be done between the specified
-- rectangle and the previous scissor rectangle transformed in the current
-- transform space. The resulting shape is always rectangle.
{#fun unsafe nvgIntersectScissor as intersectScissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Reset and disables scissoring.
{#fun unsafe nvgResetScissor as resetScissor
        {`Context'} -> `()'#}

-- | Clears the current path and sub-paths.
{#fun unsafe nvgBeginPath as beginPath
        {`Context'} -> `()'#}

-- | Starts new sub-path with specified point as first point.
{#fun unsafe nvgMoveTo as moveTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

-- | Adds line segment from the last point in the path to the specified point.
{#fun unsafe nvgLineTo as lineTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

-- | Adds cubic bezier segment from last point in the path via two control points to the specified point.
{#fun unsafe nvgBezierTo as bezierTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Adds quadratic bezier segment from last point in the path via a control point to the specified point
{#fun unsafe nvgQuadTo as quadTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Adds an arc segment at the corner defined by the last path point, and two specified points.
{#fun unsafe nvgArcTo as arcTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Closes current sub-path with a line segment.
{#fun unsafe nvgClosePath as closePath
        {`Context'} -> `()'#}

-- | Sets the current sub-path winding, see NVGwinding and NVGsolidity. 
{#fun unsafe nvgPathWinding as pathWinding
        {`Context', `CInt'} -> `()'#}

-- | Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
-- and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
-- Angles are specified in radians.
{#fun unsafe nvgArc as arc
        {`Context',`CFloat',`CFloat', `CFloat', `CFloat', `CFloat', `Winding'} -> `()'#}

-- | Creates new rectangle shaped sub-path.
{#fun unsafe nvgRect as rect
         {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new rounded rectangle shaped sub-path.
{#fun unsafe nvgRoundedRect as roundedRect
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new ellipse shaped sub-path.
{#fun unsafe nvgEllipse as ellipse
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new circle shaped sub-path. 
{#fun unsafe nvgCircle as circle
        {`Context',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Fills the current path with current fill style.
{#fun unsafe nvgFill as fill
        {`Context'} -> `()'#}

-- | Fills the current path with current stroke style.
{#fun unsafe nvgStroke as stroke
        {`Context'} -> `()'#}

safeFont :: CInt -> Maybe Font
safeFont i
  | i < 0 = Nothing
  | otherwise = Just (Font i)

-- | Creates font by loading it from the disk from specified file name.
-- Returns handle to the font.
{#fun unsafe nvgCreateFont as createFont
        {`Context',withCString*`T.Text','withCString.unwrapFileName'*`FileName'} -> `Maybe Font'safeFont#}

-- | Creates image by loading it from the specified memory chunk.
-- Returns handle to the font.
{#fun unsafe nvgCreateFontMem as createFontMem
        {`Context',withCString*`T.Text',useAsCStringLen'*`ByteString'&,zero-`CInt'} -> `Maybe Font'safeFont#}

-- | Finds a loaded font of specified name, and returns handle to it, or -1 if the font is not found.
{#fun unsafe nvgFindFont as findFont
        {`Context', withCString*`T.Text'} -> `Maybe Font'safeFont#}

-- | Sets the font size of current text style.
{#fun unsafe nvgFontSize as fontSize
        {`Context',`CFloat'} -> `()'#}

-- | Sets the blur of current text style.
{#fun unsafe nvgFontBlur as fontBlur
        {`Context',`CFloat'} -> `()'#}

-- | Sets the letter spacing of current text style.
{#fun unsafe nvgTextLetterSpacing as textLetterSpacing
        {`Context',`CFloat'} -> `()'#}

-- | Sets the proportional line height of current text style. The line height is specified as multiple of font size. 
{#fun unsafe nvgTextLineHeight as textLineHeight
        {`Context',`CFloat'} -> `()'#}

-- | Sets the text align of current text style, see NVGalign for options.
{#fun unsafe nvgTextAlign as textAlign
        {`Context',bitMask`S.Set Align'} -> `()'#}

-- | Sets the font face based on specified id of current text style.
{#fun unsafe nvgFontFaceId as fontFaceId
        {`Context',fontHandle`Font'} -> `()'#}

-- | Sets the font face based on specified name of current text styl
{#fun unsafe nvgFontFace as fontFace
        {`Context',withCString*`T.Text'} -> `()'#}

-- | Draws text string at specified location. If end is specified only the sub-string up to the end is drawn.
{#fun unsafe nvgText as text
         {`Context',`CFloat',`CFloat',id`Ptr CChar',id`Ptr CChar'} -> `()'#}

-- | Draws multi-line text string at specified location wrapped at the specified width. If end is specified only the sub-string up to the end is drawn.
-- | White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
-- | Words longer than the max width are slit at nearest character (i.e. no hyphenation).
{#fun unsafe nvgTextBox as textBox
        {`Context',`CFloat',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar'} -> `()'#}

newtype Bounds = Bounds (V4 CFloat) deriving (Show,Read,Eq,Ord)

instance Storable Bounds where
  sizeOf _ = sizeOf (0 :: CFloat) * 4
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       a <- peekElemOff p' 0
       b <- peekElemOff p' 1
       c <- peekElemOff p' 2
       d <- peekElemOff p' 3
       pure (Bounds (V4 a b c d))
  poke p (Bounds (V4 a b c d)) =
    do let p' = castPtr p :: Ptr CFloat
       pokeElemOff p' 0 a
       pokeElemOff p' 1 b
       pokeElemOff p' 2 c
       pokeElemOff p' 3 d

peekBounds :: Ptr CFloat -> IO Bounds
peekBounds = peek . castPtr

allocaBounds :: (Ptr CFloat -> IO b) -> IO b
allocaBounds f = alloca (\(p :: Ptr Bounds) -> f (castPtr p))

-- | Measures the specified text string. Parameter bounds should be a pointer to float[4],
-- if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
-- Returns the horizontal advance of the measured text (i.e. where the next character should drawn).
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextBounds as textBounds
        {`Context',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar', allocaBounds-`Bounds'peekBounds*} -> `()'#}

-- | Measures the specified multi-text string. Parameter bounds should be a pointer to float[4],
-- if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextBoxBounds as textBoxBounds
        {`Context',`CFloat',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar',allocaBounds-`Bounds'peekBounds*} -> `()'#}

-- | Calculates the glyph x positions of the specified text. If end is specified only the sub-string will be used.
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextGlyphPositions as textGlyphPositions
        {`Context',`CFloat',`CFloat',id`Ptr CChar',id`Ptr CChar',`GlyphPositionPtr', `CInt'} -> `CInt'#}

-- | Returns the vertical metrics based on the current text style.
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextMetrics as textMetrics
        {`Context',alloca-`CFloat'peek*,alloca-`CFloat'peek*,alloca-`CFloat'peek*} -> `()'#}

-- | Breaks the specified text into lines. If end is specified only the sub-string will be used.
-- White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
-- Words longer than the max width are slit at nearest character (i.e. no hyphenation).
{#fun unsafe nvgTextBreakLines as textBreakLines
        {`Context',id`Ptr CChar',id`Ptr CChar',`CFloat',`TextRowPtr',`CInt'} -> `CInt'#}

{#enum NVGcreateFlags as CreateFlags
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

bitMask :: Enum a => S.Set a -> CInt
bitMask = S.fold (.|.) 0 . S.map (fromIntegral . fromEnum)

{#fun unsafe nvgCreateGL3 as createGL3
        {bitMask`S.Set CreateFlags'} -> `Context'#}
{#fun unsafe nvgDeleteGL3 as deleteGL3
        {`Context'} -> `()'#}

type GLuint = Word32

{#fun unsafe nvglCreateImageFromHandleGL3 as createImageFromHandleGL3
        {`Context',fromIntegral`GLuint',`CInt',`CInt',`CreateFlags'} -> `Image'Image#}

{#fun unsafe nvglImageHandleGL3 as imageHandleGL3
        {`Context',imageHandle`Image'} -> `GLuint'fromIntegral#}
