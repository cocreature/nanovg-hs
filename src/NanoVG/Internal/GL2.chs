module NanoVG.Internal.GL2 where

import qualified Data.Set as S
import           Data.Word
import           Foreign.C.Types

import           NanoVG.Internal.CreateContext
import           NanoVG.Internal.Types
import           NanoVG.Internal.Context
import           NanoVG.Internal.FFIHelpers

-- For now only the GL2 backends are supported
#define NANOVG_GL2

-- We need to include this to define GLuint
#if defined(darwin_HOST_OS)
#include <OpenGL/gl.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"

{#fun unsafe nvgCreateGL2 as createGL2
        {bitMask`S.Set CreateFlags'} -> `Context' fromPointer#}
{#fun unsafe nvgDeleteGL2 as deleteGL2
        { toPointer `Context' fromPointer} -> `()'#}

{#fun unsafe nvglCreateImageFromHandleGL2 as createImageFromHandleGL2
        { toPointer `Context' fromPointer
        , fromIntegral`GLuint'
        , `CInt'
        , `CInt'
        , toCInt `CreateFlags' fromCInt
        } -> `Image'Image#}

{#fun unsafe nvglImageHandleGL2 as imageHandleGL2
        { toPointer `Context' fromPointer
        , imageHandle`Image'
        } -> `GLuint'fromIntegral#}
