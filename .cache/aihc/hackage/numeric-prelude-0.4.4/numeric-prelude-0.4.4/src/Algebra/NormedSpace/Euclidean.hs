{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Abstraction of normed vector spaces
-}

module Algebra.NormedSpace.Euclidean where

import NumericPrelude.Base
import NumericPrelude.Numeric (sqr, abs, zero, (+), sum, Float, Double, Int, Integer, )
import qualified Prelude as P

import qualified Number.Ratio as Ratio

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Absolute      as Absolute
import qualified Algebra.Module    as Module

import qualified Data.Complex as Complex98
import qualified Data.Foldable as Fold


{-|
Helper class for 'C' that does not need an algebraic type @a@.

Minimal definition:
'normSqr'
-}
class (Absolute.C a, Module.C a v) => Sqr a v where
  {-| Square of the Euclidean norm of a vector.
      This is sometimes easier to implement. -}
  normSqr :: v -> a
--  normSqr = sqr . norm

{- |
Default definition for 'normSqr' that is based on 'Fold.Foldable' class.
-}
{-# INLINE normSqrFoldable #-}
normSqrFoldable ::
   (Sqr a v, Fold.Foldable f) => f v -> a
normSqrFoldable =
   Fold.foldl (\a v -> a + normSqr v) zero

{- |
Default definition for 'normSqr' that is based on 'Fold.Foldable' class
and the argument vector has at least one component.
-}
{-# INLINE normSqrFoldable1 #-}
normSqrFoldable1 ::
   (Sqr a v, Fold.Foldable f, Functor f) => f v -> a
normSqrFoldable1 =
   Fold.foldl1 (+) . fmap normSqr


{-|
A vector space equipped with an Euclidean or a Hilbert norm.

Minimal definition:
'norm'
-}
class (Sqr a v) => C a v where
  {-| Euclidean norm of a vector. -}
  norm :: v -> a


defltNorm :: (Algebraic.C a, Sqr a v) => v -> a
defltNorm = Algebraic.sqrt . normSqr


{-* Instances for atomic types -}

instance Sqr Float Float where
  normSqr = sqr

instance C Float Float where
  norm    = abs

instance Sqr Double Double where
  normSqr = sqr

instance C Double Double where
  norm    = abs

instance Sqr Int Int where
  normSqr = sqr

instance C Int Int where
  norm    = abs

instance Sqr Integer Integer where
  normSqr = sqr

instance C Integer Integer where
  norm    = abs


{-* Instances for composed types -}

instance (Absolute.C a, PID.C a) => Sqr (Ratio.T a) (Ratio.T a) where
  normSqr = sqr

instance (Sqr a v0, Sqr a v1) => Sqr a (v0, v1) where
  normSqr (x0,x1) = normSqr x0 + normSqr x1

instance (Algebraic.C a, Sqr a v0, Sqr a v1) => C a (v0, v1) where
  norm    = defltNorm

instance (Sqr a v0, Sqr a v1, Sqr a v2) => Sqr a (v0, v1, v2) where
  normSqr (x0,x1,x2) = normSqr x0 + normSqr x1 + normSqr x2

instance (Algebraic.C a, Sqr a v0, Sqr a v1, Sqr a v2) => C a (v0, v1, v2) where
  norm    = defltNorm

instance (Sqr a v) => Sqr a [v] where
  normSqr = sum . map normSqr

instance (Algebraic.C a, Sqr a v) => C a [v] where
  norm    = defltNorm


instance (Sqr a v, P.RealFloat v) => Sqr a (Complex98.Complex v) where
  normSqr (x0 Complex98.:+ x1) = normSqr x0 + normSqr x1

instance
  (Algebraic.C a, Sqr a v, P.RealFloat v) =>
    C a (Complex98.Complex v) where
  norm    = defltNorm
