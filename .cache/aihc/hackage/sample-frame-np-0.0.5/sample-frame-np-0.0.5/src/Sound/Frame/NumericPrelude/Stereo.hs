{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sound.Frame.NumericPrelude.Stereo (
   T, left, right, cons, map, swap,
   Stereo.Channel(Stereo.Left, Stereo.Right), Stereo.select,
   Stereo.interleave,
   Stereo.sequence,
   Stereo.liftApplicative,
   ) where

import qualified Sound.Frame.Stereo as Stereo
import Sound.Frame.Stereo (T, left, right, cons, map, swap)

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import qualified Algebra.Module    as Module
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Additive  as Additive

import Algebra.Module   ((<*>.*>), )
import Algebra.Additive ((<*>.+), (<*>.-), (<*>.-$), )

import qualified NumericPrelude.Elementwise as Elem
import Control.Applicative (Applicative(pure), )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (map)
import Prelude ()


instance (Additive.C a) => Additive.C (T a) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = cons zero zero
   (+)    = Elem.run2 $ pure cons <*>.+  left <*>.+  right
   (-)    = Elem.run2 $ pure cons <*>.-  left <*>.-  right
   negate = Elem.run  $ pure cons <*>.-$ left <*>.-$ right

instance (Module.C a v) => Module.C a (T v) where
   {-# INLINE (*>) #-}
   (*>) = Elem.run2 $ pure cons <*>.*> left <*>.*> right


instance (Additive.C a, NormedSum.C a v) => NormedSum.C a (T v) where
   {-# INLINE norm #-}
   norm x =
      NormedSum.norm (left x) + NormedSum.norm (right x)

instance (NormedEuc.Sqr a v) => NormedEuc.Sqr a (T v) where
   {-# INLINE normSqr #-}
   normSqr x =
      NormedEuc.normSqr (left x) + NormedEuc.normSqr (right x)

instance (Algebraic.C a, NormedEuc.Sqr a v) => NormedEuc.C a (T v) where
   {-# INLINE norm #-}
   norm = NormedEuc.defltNorm

instance (Ord a, NormedMax.C a v) => NormedMax.C a (T v) where
   {-# INLINE norm #-}
   norm x =
      max (NormedMax.norm (left x)) (NormedMax.norm (right x))
