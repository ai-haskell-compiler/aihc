{-# options_haddock prune, ignore-exports #-}

-- |Reexports of types from [containers](https://hackage.haskell.org/package/containers) and
-- [data-default](https://hackage.haskell.org/package/data-default).
module Incipit.Libraries (
  module Data.Default,
  module Data.IntMap.Strict,
  module Data.IntSet,
  module Data.Map.Strict,
  module Data.Sequence,
  module Data.Set,
) where

import Data.Default (Default (..))
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
