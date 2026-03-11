{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Abstraction of normed vector spaces
-}

module Algebra.NormedSpace.Sum where

import NumericPrelude.Base
import NumericPrelude.Numeric
import qualified Prelude as P

import qualified Number.Ratio as Ratio

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Absolute     as Absolute
import qualified Algebra.Additive as Additive
import qualified Algebra.Module   as Module

import qualified Data.Complex as Complex98
import qualified Data.Foldable as Fold


{-|
  The super class is only needed to state the laws
  @
     v == zero        ==   norm v == zero
     norm (scale x v) ==   abs x * norm v
     norm (u+v)       <=   norm u + norm v
  @
-}
class (Absolute.C a, Module.C a v) => C a v where
  norm :: v -> a

{- |
Default definition for 'norm' that is based on 'Fold.Foldable' class.
-}
{-# INLINE normFoldable #-}
normFoldable ::
   (C a v, Fold.Foldable f) => f v -> a
normFoldable =
   Fold.foldl (\a v -> a + norm v) zero

{- |
Default definition for 'norm' that is based on 'Fold.Foldable' class
and the argument vector has at least one component.
-}
{-# INLINE normFoldable1 #-}
normFoldable1 ::
   (C a v, Fold.Foldable f, Functor f) => f v -> a
normFoldable1 =
   Fold.foldl1 (+) . fmap norm


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


instance (Absolute.C a, PID.C a) => C (Ratio.T a) (Ratio.T a) where
  norm = abs

instance (Additive.C a, C a v0, C a v1) => C a (v0, v1) where
  norm (x0,x1) = norm x0 + norm x1

instance (Additive.C a, C a v0, C a v1, C a v2) => C a (v0, v1, v2) where
  norm (x0,x1,x2) = norm x0 + norm x1 + norm x2

instance (Additive.C a, C a v) => C a [v] where
  norm = sum . map norm


instance (C a v, P.RealFloat v) => C a (Complex98.Complex v) where
  norm (x0 Complex98.:+ x1) = norm x0 + norm x1
