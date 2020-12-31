module NanoVG.Internal.GlobalComposite where

import NanoVG.Internal.Context

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGblendFactor as BlendFactor
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

{#enum NVGcompositeOperation as CompositeOperation
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

-- | Composite operation
--
-- The composite operations in NanoVG are modeled after HTML Canvas API, and
-- the blend func is based on OpenGL (see corresponding manuals for more info).
-- The colors in the blending state have premultiplied alpha.

-- | Sets the composite operation. The op parameter should be one of
-- NVGcompositeOperation.
{#fun unsafe nvgGlobalCompositeOperation as globalCompositeOperation
        {`Context',`CompositeOperation'} -> `()'#}

-- | Sets the composite operation with custom pixel arithmetic. The parameters
-- should be one of NVGblendFactor.
{#fun unsafe nvgGlobalCompositeBlendFunc as globalCompositeBlendFunc
        {`Context',`BlendFactor', `BlendFactor'} -> `()'#}

-- | Sets the composite operation with custom pixel arithmetic for RGB and alpha
-- components separately. The parameters should be one of NVGblendFactor.
{#fun unsafe nvgGlobalCompositeBlendFuncSeparate as globalCompositeBlendFuncSeparate
        {`Context',`BlendFactor', `BlendFactor', `BlendFactor', `BlendFactor'} -> `()'#}
