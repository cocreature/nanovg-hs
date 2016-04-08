module NanoVG.Internal.State where

import NanoVG.Internal.Context

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

-- | Pushes and saves the current render state into a state stack.
--
-- A matching 'restore' must be used to restore the state.
{#fun unsafe nvgSave as save
        {`Context'} -> `()'#}

-- | Pops and restores current render state.
{#fun unsafe nvgRestore as restore
        {`Context'} -> `()'#}

-- | Resets current render state to default values. Does not affect the render state stack.
{#fun unsafe nvgReset as reset
        {`Context'} -> `()'#}

