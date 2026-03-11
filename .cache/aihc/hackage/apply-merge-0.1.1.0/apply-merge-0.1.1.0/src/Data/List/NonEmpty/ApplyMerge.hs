-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.List.NonEmpty.ApplyMerge
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Data.List.NonEmpty.ApplyMerge
  ( applyMerge,
    applyMergeBy,
    applyMergeOn,
  )
where

import ApplyMerge.IntSet qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies, reflect, reify)
import Data.Semigroup (Arg (..))

-- | Like 'Data.List.ApplyMerge.applyMerge', but operates on 'NonEmpty's instead
--   of lists.
applyMerge :: (Ord c) => (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
applyMerge = ApplyMerge.IntSet.applyMergeNonEmpty

-- | Like 'applyMerge', but uses a custom comparison function.
applyMergeBy ::
  (c -> c -> Ordering) ->
  (a -> b -> c) ->
  NonEmpty a ->
  NonEmpty b ->
  NonEmpty c
applyMergeBy = applyMergeBy_

-- Reflection logic in applyMerge_ is based on "All about reflection: a
-- tutorial" [1] by Arnaud Spiwack, licensed under CC BY 4.0 [2].
--
-- [1]: https://www.tweag.io/blog/2017-12-21-reflection-tutorial/
-- [2]: https://creativecommons.org/licenses/by/4.0/
applyMergeBy_ ::
  forall a b c.
  (c -> c -> Ordering) ->
  (a -> b -> c) ->
  NonEmpty a ->
  NonEmpty b ->
  NonEmpty c
applyMergeBy_ cmp f as bs =
  reify cmp $ \(_ :: Proxy s) ->
    let f' :: a -> b -> ReflectedOrd s c
        f' a b = ReflectedOrd (f a b)
     in NonEmpty.map unReflectedOrd (applyMerge f' as bs)

newtype ReflectedOrd s a = ReflectedOrd {unReflectedOrd :: a}

instance (Reifies s (a -> a -> Ordering)) => Eq (ReflectedOrd s a) where
  (==) (ReflectedOrd x) (ReflectedOrd y) =
    let cmp = reflect (Proxy :: Proxy s)
     in cmp x y == EQ

instance (Reifies s (a -> a -> Ordering)) => Ord (ReflectedOrd s a) where
  compare (ReflectedOrd x) (ReflectedOrd y) =
    let cmp = reflect (Proxy :: Proxy s)
     in cmp x y

-- | Like 'applyMerge', but applies a custom projection function before
--   performing comparisons.
applyMergeOn ::
  (Ord d) => (c -> d) -> (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
applyMergeOn p f as bs =
  let f' a b =
        let c = f a b
         in Arg (p c) c
   in NonEmpty.map (\(Arg _ c) -> c) (applyMerge f' as bs)
