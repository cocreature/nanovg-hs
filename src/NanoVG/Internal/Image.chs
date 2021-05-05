module NanoVG.Internal.Image where

import Foreign.C.Types
import Data.ByteString hiding (null)
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Data.Set as S

import NanoVG.Internal.Context
import NanoVG.Internal.FFIHelpers
import NanoVG.Internal.Types

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGimageFlags as ImageFlags
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

safeImage :: CInt -> Maybe Image
safeImage i
  | i < 0 = Nothing
  | otherwise = Just (Image i)

-- | Creates image by loading it from the disk from specified file name.
{#fun unsafe nvgCreateImage as createImage
        {`Context','withCString.unwrapFileName'*`FileName',bitMask`S.Set ImageFlags'} -> `Maybe Image'safeImage#}

-- | Creates image by loading it from the specified chunk of memory.
{#fun unsafe nvgCreateImageMem as createImageMem
        {`Context',bitMask`S.Set ImageFlags',useAsCStringLen'*`ByteString'&} -> `Maybe Image'safeImage#}

-- | Creates image from specified image data.
{#fun unsafe nvgCreateImageRGBA as createImageRGBA
        {`Context',`CInt',`CInt',bitMask`S.Set ImageFlags',useAsPtr*`ByteString'} -> `Maybe Image'safeImage#}

-- | Updates image data specified by image handle.
{#fun unsafe nvgUpdateImage as updateImage
        {`Context',imageHandle`Image',useAsPtr*`ByteString'} -> `()'#}

-- | Returns the dimensions of a created image.
{#fun unsafe nvgImageSize as imageSize
        {`Context',imageHandle`Image',alloca-`CInt'peek*,alloca-`CInt'peek*} -> `()'#}

-- | Deletes created image.
{#fun unsafe nvgDeleteImage as deleteImage
        {`Context',imageHandle`Image'} -> `()'#}

