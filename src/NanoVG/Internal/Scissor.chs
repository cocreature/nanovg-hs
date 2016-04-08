module NanoVG.Internal.Scissor where

import Foreign.C.Types

import NanoVG.Internal.Context

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

-- | Sets the current scissor rectangle.
-- The scissor rectangle is transformed by the current transform.
{#fun unsafe nvgScissor as scissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Intersects current scissor rectangle with the specified rectangle.
-- The scissor rectangle is transformed by the current transform.
-- Note: in case the rotation of previous scissor rect differs from
-- the current one, the intersection will be done between the specified
-- rectangle and the previous scissor rectangle transformed in the current
-- transform space. The resulting shape is always rectangle.
{#fun unsafe nvgIntersectScissor as intersectScissor
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Reset and disables scissoring.
{#fun unsafe nvgResetScissor as resetScissor
        {`Context'} -> `()'#}
