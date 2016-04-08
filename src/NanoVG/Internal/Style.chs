module NanoVG.Internal.Style where

import Foreign.C.Types
import Foreign.Marshal.Utils

import NanoVG.Internal.Context
import NanoVG.Internal.Color
import NanoVG.Internal.Paint

#include "nanovg.h"
#include "nanovg_wrapper.h"

{#pointer *NVGcontext as Context newtype nocode#}
{#pointer *NVGcolor as ColorPtr -> Color nocode#}
{#pointer *NVGpaint as PaintPtr -> Paint nocode#}

{#enum NVGlineCap as LineCap
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

-- | Sets current stroke style to a solid color.
{#fun unsafe nvgStrokeColor_ as strokeColor
        {`Context',with*`Color'} -> `()'#}

-- | Sets current stroke style to a paint, which can be a one of the gradients or a pattern.
{#fun unsafe nvgStrokePaint_ as strokePaint
        {`Context',with*`Paint'} -> `()'#}

-- | Sets current fill style to a solid color.
{#fun unsafe nvgFillColor_ as fillColor
        {`Context',with*`Color'} -> `()'#}

-- | Sets current fill style to a paint, which can be a one of the gradients or a pattern.
{#fun unsafe nvgFillPaint_ as fillPaint
        {`Context',with*`Paint'} -> `()'#}

-- | Sets the miter limit of the stroke style.
-- Miter limit controls when a sharp corner is beveled.
{#fun unsafe nvgMiterLimit as miterLimit
        {`Context',`CFloat'} -> `()'#}

-- | Sets the stroke width of the stroke style.
{#fun unsafe nvgStrokeWidth as strokeWidth
        {`Context',`CFloat'} -> `()'#}

-- | Sets how the end of the line (cap) is drawn,
-- Can be one of: 'Butt' (default), 'Round', 'Square'.
{#fun unsafe nvgLineCap as lineCap
        {`Context',`LineCap'} -> `()'#}

-- | Sets how sharp path corners are drawn.
-- Can be one of 'Miter' (default), 'Round', 'Bevel.
{#fun unsafe nvgLineJoin as lineJoin
        {`Context',`LineCap'} -> `()'#}

-- | Sets the transparency applied to all rendered shapes.
-- Already transparent paths will get proportionally more transparent as well.
{#fun unsafe nvgGlobalAlpha as globalAlpha
        {`Context',`CFloat'} -> `()'#}

