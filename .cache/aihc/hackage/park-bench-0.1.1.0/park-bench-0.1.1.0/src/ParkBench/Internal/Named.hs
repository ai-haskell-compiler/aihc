module ParkBench.Internal.Named
  ( Named (..),
  )
where

import ParkBench.Internal.Prelude

-- | A named thing.
data Named a = Named
  { name :: {-# UNPACK #-} !Text,
    thing :: !a
  }
  deriving stock (Foldable, Functor, Traversable)
