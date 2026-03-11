-- | This module provides sequence-specific multimap functionality.
module Data.Multimap.Seq (
  SeqMultimap,
  popFirst, popLast
) where

import Data.Functor.Compose (Compose(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Multimap.Generic (Multimap, modifyManyF)

-- | A multimap with 'Seq' values.
--
-- See "Data.Multimap.Seq" for operations specific to this type.
type SeqMultimap = Multimap Seq

-- | /O(log m)/ Pops the first value associated with a key, if present.
popFirst :: Ord k => k -> SeqMultimap k v -> Maybe (v, SeqMultimap k v)
popFirst k = getCompose . modifyManyF (Compose . go . Seq.viewl) k where
  go Seq.EmptyL = Nothing
  go (v Seq.:< c) = Just (v, c)

-- | /O(log m)/ Pops the last value associated with a key, if present.
popLast :: Ord k => k -> SeqMultimap k v -> Maybe (v, SeqMultimap k v)
popLast k = getCompose . modifyManyF (Compose . go . Seq.viewr) k where
  go Seq.EmptyR = Nothing
  go (c Seq.:> v) = Just (v, c)
