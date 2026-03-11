{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Abstraction of normed vector spaces
-}

module Algebra.NormedSpace.Maximum where

import NumericPrelude.Base
import NumericPrelude.Numeric
import qualified Prelude as P

import qualified Number.Ratio as Ratio

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.RealRing as RealRing
import qualified Algebra.Module   as Module

import qualified Data.Complex as Complex98
import qualified Data.Foldable as Fold


class (RealRing.C a, Module.C a v) => C a v where
  norm :: v -> a

{- |
Default definition for 'norm' that is based on 'Fold.Foldable' class.
-}
{-# INLINE normFoldable #-}
normFoldable ::
   (C a v, Fold.Foldable f) => f v -> a
normFoldable =
   Fold.foldl (\a v -> max a (norm v)) zero

{- |
Default definition for 'norm' that is based on 'Fold.Foldable' class
and the argument vector has at least one component.
-}
{-# INLINE normFoldable1 #-}
normFoldable1 ::
   (C a v, Fold.Foldable f, Functor f) => f v -> a
normFoldable1 =
   Fold.foldl1 max . fmap norm

{-
instance (Ring.C a, Algebra.Module a a) => C a a where
  norm = abs
-}
instance C Float Float where
  norm = abs

instance C Double Double where
  norm = abs

instance C Int Int where
  norm = abs

instance C Integer Integer where
  norm = abs


instance (RealRing.C a, ToInteger.C a, PID.C a) => C (Ratio.T a) (Ratio.T a) where
  norm = abs

instance (Ord a, C a v0, C a v1) => C a (v0, v1) where
  norm (x0,x1) = max (norm x0) (norm x1)

instance (Ord a, C a v0, C a v1, C a v2) => C a (v0, v1, v2) where
  norm (x0,x1,x2) = (norm x0) `max` (norm x1) `max` (norm x2)

instance (Ord a, C a v) => C a [v] where
  norm = foldl max zero . map norm
{-
Since the norm is always non-negative,
we can use zero as identity element.
  norm = maximum . map norm
-}


instance (C a v, P.RealFloat v) => C a (Complex98.Complex v) where
  norm (x0 Complex98.:+ x1) = max (norm x0) (norm x1)
