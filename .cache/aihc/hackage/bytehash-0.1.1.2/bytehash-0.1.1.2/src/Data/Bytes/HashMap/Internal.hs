{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Bytes.HashMap.Internal
  ( Map (..)
  ) where

import Data.Int (Int32)
import Data.Primitive (ByteArray, PrimArray, SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)

{- | A static perfect hash table where the keys are byte arrays. This
  table cannot be updated after its creation, but all lookups have
  guaranteed O(1) worst-case cost. It consumes linear space. This
  is an excellent candidate for use with compact regions.
-}
data Map v
  = Map
      !ByteArray -- top-level entropy
      !(UnliftedArray ByteArray) -- entropies
      !(PrimArray Int32) -- offset to apply to hash, could probably be 32 bits
      !(UnliftedArray ByteArray) -- keys
      !(SmallArray v) -- values
  deriving stock (Functor, Foldable, Traversable)
