{-# LANGUAGE DefaultSignatures #-}

-- | This module exposes the base class used to power multimap functionality. You should not need to
-- be aware of it unless you are interested in adding a new specific multimap type.
module Data.Multimap.Collection (
  Collection(..)
) where

import Prelude hiding (filter)
import qualified Prelude as Prelude

import Data.Foldable (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- | A lower bound for multimap values. By creating an instance of this class, you can use multimap
-- operations with a custom type. An alternative could have been to use 'Applicative' but that would
-- have precluded common implementations like 'Set'.
class Foldable c => Collection c where
  -- | Creates a singleton collection.
  singleton :: v -> c v
  default singleton :: Applicative c => v -> c v
  singleton = pure

  filter :: (v -> Bool) -> c v -> c v

  -- | Returns the size of the collection. The default implementation folds over the entire
  -- structure and is /O(n)/.
  size :: c v -> Int
  size = foldl' (\n _ -> n + 1) 0

  -- | Checks whether the collection is empty. The default implementation lazily folds over the
  -- structure.
  null :: c v -> Bool
  null = foldr (\_ _ -> False) True

instance Collection [] where
  filter = Prelude.filter

instance Collection Seq where
  size = Seq.length
  filter = Seq.filter
  null = Seq.null

instance Collection Set where
  singleton = Set.singleton
  filter = Set.filter
  size = Set.size
  null = Set.null
