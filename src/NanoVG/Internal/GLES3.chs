module NanoVG.Internal.GLES3 where

import qualified Data.Set as S
import           Data.Word
import           Foreign.C.Types

import           NanoVG.Internal.CreateContext
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

{#fun unsafe nvgCreateGLES3 as createGLES3
        {bitMask`S.Set CreateFlags'} -> `Context' fromPointer#}
{#fun unsafe nvgDeleteGLES3 as deleteGLES3
        {toPointer `Context' fromPointer} -> `()'#}

{#fun unsafe nvglCreateImageFromHandleGLES3 as createImageFromHandleGLES3
        { toPointer `Context' fromPointer
        , fromIntegral`GLuint'
        , `CInt'
        , `CInt'
        , toCInt `CreateFlags' fromCInt
        } -> `Image'Image#}

{#fun unsafe nvglImageHandleGLES3 as imageHandleGLES3
        { toPointer `Context' fromPointer
        , imageHandle`Image'
        } -> `GLuint'fromIntegral#}
