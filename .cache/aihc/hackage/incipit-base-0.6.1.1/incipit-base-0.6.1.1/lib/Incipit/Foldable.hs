-- |Overrides for problematic 'Foldable' functions.
module Incipit.Foldable (
  module Incipit.Foldable,
  module Data.Foldable,
) where

import qualified Data.Foldable as Foldable
import Data.Foldable (
  Foldable (elem, fold, foldMap, foldMap', foldl', foldl1, foldr, foldr', foldr1, length, null, product, sum, toList),
  all,
  and,
  any,
  asum,
  concat,
  concatMap,
  find,
  foldlM,
  forM_,
  for_,
  mapM_,
  or,
  sequenceA_,
  sequence_,
  traverse_,
  )
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord, Ordering)

-- |Default to using the strict version since the lazy one is so controversial.
foldl ::
  ∀ t b a .
  Foldable t =>
  (b -> a -> b) ->
  b ->
  t a ->
  b
foldl =
  Foldable.foldl'
{-# inlineable foldl #-}

-- |Safe version of 'Foldable.minimum'.
minimum ::
  ∀ t a .
  Ord a =>
  Foldable t =>
  t a ->
  Maybe a
minimum ta =
  if null ta
  then Nothing
  else Just (Foldable.minimum ta)
{-# inlineable minimum #-}

-- |Safe version of 'Foldable.minimumBy'.
minimumBy ::
  ∀ t a .
  Foldable t =>
  (a -> a -> Ordering) ->
  t a ->
  Maybe a
minimumBy cmp ta =
  if null ta
  then Nothing
  else Just (Foldable.minimumBy cmp ta)
{-# inlineable minimumBy #-}

-- |Safe version of 'Foldable.maximum'.
maximum ::
  ∀ t a .
  Ord a =>
  Foldable t =>
  t a ->
  Maybe a
maximum ta =
  if null ta
  then Nothing
  else Just (Foldable.maximum ta)
{-# inlineable maximum #-}

-- |Safe version of 'Foldable.maximumBy'.
maximumBy ::
  ∀ t a .
  Foldable t =>
  (a -> a -> Ordering) ->
  t a ->
  Maybe a
maximumBy cmp ta =
  if null ta
  then Nothing
  else Just (Foldable.maximumBy cmp ta)
{-# inlineable maximumBy #-}
