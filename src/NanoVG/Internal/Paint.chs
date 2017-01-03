{-# LANGUAGE RecordWildCards #-}
module NanoVG.Internal.Paint where

import Control.Applicative (pure)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import NanoVG.Internal.Color
import NanoVG.Internal.Context
import NanoVG.Internal.FixedVector
import NanoVG.Internal.Transformation
import NanoVG.Internal.Types

#include "nanovg.h"
#include "nanovg_wrapper.h"

{#pointer *NVGcontext as Context newtype nocode#}
{#pointer *NVGcolor as ColorPtr -> Color nocode#}
{#pointer *NVGpaint as PaintPtr -> Paint#}

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

data Paint =
  Paint {xform :: Transformation
        ,extent :: Extent
        ,radius :: !CFloat
        ,feather :: !CFloat
        ,innerColor :: !Color
        ,outerColor :: !Color
        ,image :: !Image} deriving (Show,Read,Eq,Ord)

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
