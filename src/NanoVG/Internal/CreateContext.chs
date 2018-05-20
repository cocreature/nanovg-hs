module NanoVG.Internal.CreateContext where

import Foreign.C.Types
import Data.Word (Word32(..))

#include "nanovg.h"
#include "nanovg_gl.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGcreateFlags as CreateFlags
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

type GLuint = Word32

toCInt :: CreateFlags -> CInt
toCInt = fromIntegral . fromEnum

fromCInt :: CInt -> CreateFlags
fromCInt = toEnum . fromIntegral
