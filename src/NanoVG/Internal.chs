{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module NanoVG.Internal where

import           Data.Bits
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

withCString :: T.Text -> (CString -> IO b) -> IO b
withCString t = useAsCString (T.encodeUtf8 t)

useAsCStringLen' :: ByteString -> ((Ptr CUChar,CInt) -> IO a) -> IO a
useAsCStringLen' bs f = useAsCStringLen bs (\(ptr,len) -> f (castPtr ptr,fromIntegral len))

useAsPtr :: ByteString -> (Ptr CUChar -> IO a) -> IO a
useAsPtr bs f = useAsCStringLen bs (\(ptr,_) -> f (castPtr ptr))

zero :: Num a => (a -> b) -> b
zero f = f 0

null :: (Ptr a -> b) -> b
null f = f nullPtr

newtype FileName = FileName { unwrapFileName :: T.Text }

newtype Image = Image {imageHandle :: CInt} deriving (Show,Read,Eq,Ord)

newtype Font = Font {fontHandle :: CInt} deriving (Show,Read,Eq,Ord)

{#pointer *NVGcontext as Context newtype#}

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
  sizeOf _ = sizeOf (0 :: CFloat) * 4
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
     GlyphPosition { str :: !(Ptr CChar)
                   , glyphX :: !CFloat
                   -- Remove prefix once GHC 8 is released
                   , glyphPosMinX :: !CFloat
                   , glyphPosMaxX :: !CFloat}

{#pointer *NVGglyphPosition as GlyphPositionPtr -> GlyphPosition#}

instance Storable GlyphPosition where
  sizeOf _ = {#sizeof NVGglyphPosition#}
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
  TextRow {start :: !(Ptr CChar)
          ,end :: !(Ptr CChar)
          ,next :: !(Ptr CChar)
          ,width :: !CFloat
          -- Remove prefix once GHC 8 is released
          ,textRowMinX :: !CFloat
          ,textRowMaxX :: !CFloat} deriving (Show,Eq,Ord)

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

{#fun unsafe nvgBeginFrame as beginFrame
        {`Context',`CInt',`CInt',`Float'} -> `()'#}

{#fun unsafe nvgCancelFrame as canelFrame
        {`Context'} -> `()'#}

{#fun unsafe nvgEndFrame as endFrame
        {`Context'} -> `()'#}

{#fun pure unsafe nvgRGB_ as rgb
        {id`CUChar',id`CUChar',id`CUChar',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgRGBf_ as rgbf
        {`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgRGBA_ as rgba
        {id`CUChar',id`CUChar',id`CUChar',id`CUChar',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgRGBAf_ as rgbaf
        {`CFloat',`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgLerpRGBA_ as lerpRGBA
        {with*`Color',with*`Color',`CFloat',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgTransRGBA_ as transRGBA
        {with*`Color',id`CUChar',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgTransRGBAf_ as transRGBAf
        {with*`Color',`CFloat',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgHSL_ as hsl
        {`CFloat',`CFloat',`CFloat',alloca-`Color'peek*} -> `()'#}

{#fun pure unsafe nvgHSLA_ as hsla
        {`CFloat',`CFloat',`CFloat',id`CUChar',alloca-`Color'peek*} -> `()'#}

{#fun unsafe nvgSave as save
        {`Context'} -> `()'#}

{#fun unsafe nvgRestore as restore
        {`Context'} -> `()'#}

{#fun unsafe nvgReset as reset
        {`Context'} -> `()'#}

{#fun unsafe nvgStrokeColor_ as strokeColor
        {`Context',with*`Color'} -> `()'#}

{#fun unsafe nvgStrokePaint_ as strokePaint
        {`Context',with*`Paint'} -> `()'#}

{#fun unsafe nvgFillColor_ as fillColor
        {`Context',with*`Color'} -> `()'#}

{#fun unsafe nvgFillPaint_ as fillPaint
        {`Context',with*`Paint'} -> `()'#}

{#fun unsafe nvgMiterLimit as miterLimit
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgStrokeWidth as strokeWidth
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgLineCap as lineCap
        {`Context',`LineCap'} -> `()'#}

{#fun unsafe nvgLineJoin as lineJoin
        {`Context',`LineCap'} -> `()'#}

{#fun unsafe nvgGlobalAlpha as globalAlpha
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgResetTransform as resetTransform
        {`Context'} -> `()'#}

{#fun unsafe nvgTransform as transform
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgTranslate as translate
        {`Context',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgRotate as rotate
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgSkewX as skewX
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgSkewY as skewY
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgScale as scale
        {`Context',`CFloat',`CFloat'} -> `()'#}

peekTransformation :: Ptr CFloat -> IO Transformation
peekTransformation = peek . castPtr

allocaTransformation :: (Ptr CFloat -> IO b) -> IO b
allocaTransformation f = alloca (\(p :: Ptr Transformation) -> f (castPtr p))

withTransformation :: Transformation -> (Ptr CFloat -> IO b) -> IO b
withTransformation t f = with t (\p -> f (castPtr p)) 

{#fun unsafe nvgCurrentTransform as currentTransform
        {`Context',allocaTransformation-`Transformation'peekTransformation*} -> `()'#}

{#fun unsafe nvgTransformIdentity as transformIdentity
        {allocaTransformation-`Transformation'peekTransformation*} -> `()'#}

{#fun unsafe nvgTransformTranslate as transformTranslate
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgTransformScale as transformScale
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgTransformRotate as transformRotate
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

{#fun unsafe nvgTransformSkewX as transformSkewX
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

{#fun unsafe nvgTransformSkewY as transformSkewY
        {allocaTransformation-`Transformation'peekTransformation*,`CFloat'} -> `()'#}

{#fun unsafe nvgTransformMultiply as transformMultiply
        {allocaTransformation-`Transformation'peekTransformation*,withTransformation*`Transformation'} -> `()'#}

{#fun unsafe nvgTransformInverse as transformInverse
        {allocaTransformation-`Transformation'peekTransformation*,withTransformation*`Transformation'} -> `()'#}

{#fun pure unsafe nvgTransformPoint as transformPoint
        {alloca-`CFloat'peek*,alloca-`CFloat'peek*,withTransformation*`Transformation',`CFloat',`CFloat'} -> `()'#}

{#fun pure unsafe nvgDegToRad as degtoRad
        {`CFloat'} -> `CFloat'#}

{#fun pure unsafe nvgRadToDeg as radToDeg
        {`CFloat'} -> `CFloat'#}

{#fun unsafe nvgCreateImage as createImage
        {`Context','withCString.unwrapFileName'*`FileName',`CInt'} -> `Image'Image#}

{#fun unsafe nvgCreateImageMem as createImageMem
        {`Context',`ImageFlags',useAsCStringLen'*`ByteString'&} -> `Image'Image#}

{#fun unsafe nvgCreateImageRGBA as createImageRGBA
        {`Context',`CInt',`CInt',`ImageFlags',useAsPtr*`ByteString'} -> `Image'Image#}

{#fun unsafe nvgUpdateImage as updateImage
        {`Context',imageHandle`Image',useAsPtr*`ByteString'} -> `()'#}

{#fun unsafe nvgImageSize as imageSize
        {`Context',imageHandle`Image',alloca-`CInt'peek*,alloca-`CInt'peek*} -> `()'#}

{#fun unsafe nvgDeleteImage as deleteImage
        {`Context',imageHandle`Image'} -> `()'#}

{#fun unsafe nvgLinearGradient_ as linearGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

{#fun unsafe nvgBoxGradient_ as boxGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

{#fun unsafe nvgRadialGradient_ as radialGradient
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',with*`Color',with*`Color',alloca-`Paint'peek*} -> `()'#}

{#fun unsafe nvgImagePattern_ as imagePattern
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',imageHandle`Image',`CFloat',alloca-`Paint'peek*} -> `()'#}

{#fun unsafe nvgScissor as scissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgIntersectScissor as intersectScissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgResetScissor as resetScissor
        {`Context'} -> `()'#}

{#fun unsafe nvgBeginPath as beginPath
        {`Context'} -> `()'#}

{#fun unsafe nvgMoveTo as moveTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgLineTo as lineTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgBezierTo as bezierTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgQuadTo as quadTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgArcTo as arcTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgClosePath as closePath
        {`Context'} -> `()'#}

{#fun unsafe nvgPathWinding as pathWinding
        {`Context', `CInt'} -> `()'#}

{#fun unsafe nvgArc as arc
        {`Context',`CFloat',`CFloat', `CFloat', `CFloat', `CFloat', `Winding'} -> `()'#}

{#fun unsafe nvgRect as rect
         {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgRoundedRect as roundedRect
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgEllipse as ellipse
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgCircle as circle
        {`Context',`CFloat',`CFloat',`CFloat'} -> `()'#}

{#fun unsafe nvgFill as fill
        {`Context'} -> `()'#}

{#fun unsafe nvgStroke as stroke
        {`Context'} -> `()'#}

{#fun unsafe nvgCreateFont as createFont
        {`Context',withCString*`T.Text','withCString.unwrapFileName'*`FileName'} -> `Font'Font#}

{#fun unsafe nvgCreateFontMem as createFontMem
        {`Context',withCString*`T.Text',useAsCStringLen'*`ByteString'&,zero-`CInt'} -> `Font'Font#}

{#fun unsafe nvgFindFont as findFont
        {`Context', withCString*`T.Text'} -> `Font'Font#}

{#fun unsafe nvgFontSize as fontSize
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgFontBlur as fontBlur
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgTextLetterSpacing as textLetterSpacing
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgTextLineHeight as textLineHeight
        {`Context',`CFloat'} -> `()'#}

{#fun unsafe nvgTextAlign as textAlign
        {`Context',bitMask`S.Set Align'} -> `()'#}

{#fun unsafe nvgFontFaceId as fontFaceId
        {`Context',fontHandle`Font'} -> `()'#}

{#fun unsafe nvgFontFace as fontFace
        {`Context',withCString*`T.Text'} -> `()'#}

{#fun unsafe nvgText as text
         {`Context',`CFloat',`CFloat',id`Ptr CChar',id`Ptr CChar'} -> `()'#}

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

withBounds :: Bounds -> (Ptr CFloat -> IO b) -> IO b
withBounds t f = with t (\p -> f (castPtr p)) 

{#fun unsafe nvgTextBounds as textBounds
        {`Context',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar', allocaBounds-`Bounds'peekBounds*} -> `()'#}

{#fun unsafe nvgTextBoxBounds as textBoxBounds
        {`Context',`CFloat',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar',allocaBounds-`Bounds'peekBounds*} -> `()'#}

-- TODO: This should probably take a vector
{#fun unsafe nvgTextGlyphPositions as textGlyphPositions
        {`Context',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar',`GlyphPositionPtr', `CInt'} -> `CInt'#}

{#fun unsafe nvgTextMetrics as textMetrics
        {`Context',alloca-`CFloat'peek*,alloca-`CFloat'peek*,alloca-`CFloat'peek*} -> `()'#}

-- TODO: This should probably take a vector
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
