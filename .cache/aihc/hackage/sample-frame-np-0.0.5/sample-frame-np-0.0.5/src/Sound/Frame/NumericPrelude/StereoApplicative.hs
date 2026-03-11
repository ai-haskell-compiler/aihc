{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Define some instance for NumericPrelude.Numeric type classes
in terms of Applicative and Foldable.
This is more elegant, but maybe not faster.
-}
module Sound.Frame.NumericPrelude.StereoApplicative (T, left, right, cons, map, ) where

import Sound.Frame.Stereo (T, left, right, cons, map, )

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import qualified Algebra.Module    as Module
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Additive  as Additive

import Control.Applicative (Applicative(pure), liftA2, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (map)
import Prelude ()


instance (Additive.C a) => Additive.C (T a) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = pure zero
   (+)    = liftA2 (+)
   (-)    = liftA2 (-)
   negate = fmap negate

instance (Module.C a v) => Module.C a (T v) where
   {-# INLINE (*>) #-}
   a*>v = fmap (a*>) v


instance (Additive.C a, NormedSum.C a v) => NormedSum.C a (T v) where
   {-# INLINE norm #-}
   norm = NormedSum.normFoldable1

instance (NormedEuc.Sqr a v) => NormedEuc.Sqr a (T v) where
   {-# INLINE normSqr #-}
   normSqr = NormedEuc.normSqrFoldable1

instance (Algebraic.C a, NormedEuc.Sqr a v) => NormedEuc.C a (T v) where
   {-# INLINE norm #-}
   norm = NormedEuc.defltNorm

instance (Ord a, NormedMax.C a v) => NormedMax.C a (T v) where
   {-# INLINE norm #-}
   norm = NormedMax.normFoldable1
