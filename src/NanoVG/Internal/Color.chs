module NanoVG.Internal.Color where

import Control.Applicative (pure)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

#include "nanovg_wrapper.h"

{#pointer *NVGcolor as ColorPtr -> Color#}

-- | rgba
data Color = Color !CFloat !CFloat !CFloat !CFloat deriving (Show,Read,Eq,Ord)

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

