module NanoVG.Internal.Context
  (Context(..)
  , toPointer
  , fromPointer
  ) where

import Foreign.Ptr

-- | Opaque context that needs to be passed around
newtype Context = Context (Ptr Context)

-- | In marshaller for c2hs
toPointer :: Context -> Ptr ()
toPointer (Context p) = castPtr p

-- | out marshaller for c2hs
fromPointer :: Ptr () -> Context
fromPointer p = Context (castPtr p)
