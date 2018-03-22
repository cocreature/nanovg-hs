module NanoVG.Internal.GL3 where

import qualified Data.Set as S
import           Data.Word
import           Foreign.C.Types

import           NanoVG.Internal.CreateContext
import           NanoVG.Internal.Types
import           NanoVG.Internal.Context
import           NanoVG.Internal.FFIHelpers

-- For now only the GL3 and the GLES3 backends are supported
#define NANOVG_GL3

-- We need to include this to define GLuint
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"

{#fun unsafe nvgCreateGL3 as createGL3
        {bitMask`S.Set CreateFlags'} -> `Context' fromPointer#}
{#fun unsafe nvgDeleteGL3 as deleteGL3
        { toPointer `Context' fromPointer} -> `()'#}

{#fun unsafe nvglCreateImageFromHandleGL3 as createImageFromHandleGL3
        { toPointer `Context' fromPointer
        , fromIntegral`GLuint'
        , `CInt'
        , `CInt'
        , toCInt `CreateFlags' fromCInt
        } -> `Image'Image#}

{#fun unsafe nvglImageHandleGL3 as imageHandleGL3
        { toPointer `Context' fromPointer
        , imageHandle`Image'
        } -> `GLuint'fromIntegral#}
