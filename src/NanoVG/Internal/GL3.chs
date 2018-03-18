module NanoVG.Internal.GL3 where

#ifndef GLES3

import qualified Data.Set as S
import           Data.Word
import           Foreign.C.Types

import           NanoVG.Internal.Types
import           NanoVG.Internal.Context
import           NanoVG.Internal.FFIHelpers

-- For now only the GL3 and the GLES3 backends are supported
#define NANOVG_GL3 1
#define NANOVG_GL_USE_UNIFORMBUFFER
-- We need to include this to define GLuint
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGcreateFlags as CreateFlags
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

{#fun unsafe nvgCreateGL3 as createGL3
        {bitMask`S.Set CreateFlags'} -> `Context'#}
{#fun unsafe nvgDeleteGL3 as deleteGL3
        {`Context'} -> `()'#}

type GLuint = Word32

{#fun unsafe nvglCreateImageFromHandleGL3 as createImageFromHandleGL3
        {`Context',fromIntegral`GLuint',`CInt',`CInt',`CreateFlags'} -> `Image'Image#}

{#fun unsafe nvglImageHandleGL3 as imageHandleGL3
        {`Context',imageHandle`Image'} -> `GLuint'fromIntegral#}

#endif
