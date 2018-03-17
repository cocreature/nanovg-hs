module NanoVG.Internal.GLES3 where

import qualified Data.Set as S
import           Data.Word
import           Foreign.C.Types

import           NanoVG.Internal.GL3 (CreateFlags(..))
import           NanoVG.Internal.Types
import           NanoVG.Internal.Context
import           NanoVG.Internal.FFIHelpers

-- For now only the GL3 and GLES3 backends are supported
#define NANOVG_GLES3
-- We need to include this to define GLuint
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"

{#pointer *NVGcontext as Context newtype nocode#}

-- {#enum NVGcreateFlags as CreateFlags
--          {underscoreToCase} with prefix = "NVG_"
--          deriving (Show,Read,Eq,Ord)#}

{#fun unsafe nvgCreateGLES3 as createGLES3
        {bitMask`S.Set CreateFlags'} -> `Context'#}
{#fun unsafe nvgDeleteGLES3 as deleteGLES3
        {`Context'} -> `()'#}

type GLuint = Word32

toCInt :: CreateFlags -> CInt
toCInt = fromIntegral . fromEnum

fromCInt :: CInt -> CreateFlags
fromCInt = toEnum . fromIntegral

{#fun unsafe nvglCreateImageFromHandleGLES3 as createImageFromHandleGLES3
        { `Context'
        , fromIntegral`GLuint'
        , `CInt'
        , `CInt'
        , toCInt `CreateFlags' fromCInt
        } -> `Image'Image#}

{#fun unsafe nvglImageHandleGLES3 as imageHandleGLES3
        {`Context',imageHandle`Image'} -> `GLuint'fromIntegral#}
