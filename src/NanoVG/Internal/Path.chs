module NanoVG.Internal.Path where

import Foreign.C.Types

import NanoVG.Internal.Context

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGwinding as Winding
         {} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

-- | Clears the current path and sub-paths.
{#fun unsafe nvgBeginPath as beginPath
        {`Context'} -> `()'#}

-- | Starts new sub-path with specified point as first point.
{#fun unsafe nvgMoveTo as moveTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

-- | Adds line segment from the last point in the path to the specified point.
{#fun unsafe nvgLineTo as lineTo
        {`Context',`CFloat',`CFloat'} -> `()'#}

-- | Adds cubic bezier segment from last point in the path via two control points to the specified point.
{#fun unsafe nvgBezierTo as bezierTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Adds quadratic bezier segment from last point in the path via a control point to the specified point
{#fun unsafe nvgQuadTo as quadTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Adds an arc segment at the corner defined by the last path point, and two specified points.
{#fun unsafe nvgArcTo as arcTo
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Closes current sub-path with a line segment.
{#fun unsafe nvgClosePath as closePath
        {`Context'} -> `()'#}

-- | Sets the current sub-path winding, see NVGwinding and NVGsolidity. 
{#fun unsafe nvgPathWinding as pathWinding
        {`Context', `CInt'} -> `()'#}

-- | Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
-- and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
-- Angles are specified in radians.
{#fun unsafe nvgArc as arc
        {`Context',`CFloat',`CFloat', `CFloat', `CFloat', `CFloat', `Winding'} -> `()'#}

-- | Creates new rectangle shaped sub-path. Receives x, y, w and h
{#fun unsafe nvgRect as rect
         {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new rounded rectangle shaped sub-path. In addition to x, y, w and h, it receives
-- | r, indicating the radius (equal in all corners)
{#fun unsafe nvgRoundedRect as roundedRect
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new rounded rectangle shaped sub-path. In addition to x, y, w and h, it receives
-- | rtl, rtr, rbr, rbl, the radius of each corner in clockwise order starting from top left
{#fun unsafe nvgRoundedRectVarying as roundedRectVarying
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new ellipse shaped sub-path.
{#fun unsafe nvgEllipse as ellipse
        {`Context',`CFloat',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Creates new circle shaped sub-path. 
{#fun unsafe nvgCircle as circle
        {`Context',`CFloat',`CFloat',`CFloat'} -> `()'#}

-- | Fills the current path with current fill style.
{#fun unsafe nvgFill as fill
        {`Context'} -> `()'#}

-- | Fills the current path with current stroke style.
{#fun unsafe nvgStroke as stroke
        {`Context'} -> `()'#}
