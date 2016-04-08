module NanoVG.Internal.Context
  (Context(..)
  ) where

import Foreign.Ptr

-- | Opaque context that needs to be passed around
newtype Context = Context (Ptr Context)
