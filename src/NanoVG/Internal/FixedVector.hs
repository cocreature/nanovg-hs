{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module NanoVG.Internal.FixedVector where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | Vector of 2 strict elements
data V2 a =
  V2 !a
     !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- | Vector of 3 strict elements
data V3 a =
  V3 !a
     !a
     !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- | Vector of 4 strict elements
data V4 a =
  V4 !a
     !a
     !a
     !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- | Type synonym for 2x3 matrices
type M23 a = V2 (V3 a)
