module NanoVG.Internal.CreateFlags where

#include "nanovg_gl.h"

{#enum NVGcreateFlags as CreateFlags
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

type GLuint = Word32

toCInt :: CreateFlags -> CInt
toCInt = fromIntegral . fromEnum

fromCInt :: CInt -> CreateFlags
fromCInt = toEnum . fromIntegral
