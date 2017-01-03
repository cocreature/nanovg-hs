{-# LANGUAGE ScopedTypeVariables #-}
module NanoVG.Internal.Transformation where

import Control.Applicative (pure)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import NanoVG.Internal.Context
import NanoVG.Internal.FixedVector

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

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
