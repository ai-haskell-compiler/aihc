{-# LANGUAGE NoImplicitPrelude #-}
{- |
Plain interpolation functions.
-}
module Synthesizer.Interpolation.Core (
   linear,
   cubic,
   cubicAlt,
   ) where

import qualified Algebra.Module    as Module
import qualified Algebra.Field     as Field

import Synthesizer.Utility (affineComb, )

import NumericPrelude.Numeric
import NumericPrelude.Base



{-# INLINE linear #-}
linear ::
   (Module.C a v) =>
   v -> v -> a -> v
linear x0 x1 phase = affineComb phase (x0,x1)

{-# INLINE cubic #-}
cubic ::
   (Module.C a v, Field.C a) =>
   v -> v -> v -> v -> a -> v
cubic xm1 x0 x1 x2 t =
   let lipm12 = affineComb t (xm1,x2)
       lip01  = affineComb t (x0, x1)
       three  = 3 `asTypeOf` t
   in  lip01 + (t*(t-1)/2) *>
                  (lipm12 + (x0+x1) - three *> lip01)

{- |
The interpolators for module operations
do not simply compute a straight linear combination of some vectors.
Instead they add then scale, then add again, and so on.
This is efficient whenever scaling and addition is cheap.
In this case they might save multiplications.
I can't say much about numeric cancellations, however.
-}
{-# INLINE cubicAlt #-}
cubicAlt ::
   (Module.C a v, Field.C a) =>
   v -> v -> v -> v -> a -> v
cubicAlt xm1 x0 x1 x2 t =
   let half = 1/2 `asTypeOf` t
   in  cubicHalf    t  x0 (half *> (x1-xm1)) +
       cubicHalf (1-t) x1 (half *> (x0-x2))

{- |
@\t -> cubicHalf t x x'@ has a double zero at 1 and
at 0 it has value x and slope x'.
-}
{-# INLINE cubicHalf #-}
cubicHalf :: (Module.C t y) => t -> y -> y -> y
cubicHalf t x x' =
   (t-1)^2 *> ((1+2*t)*>x + t*>x')
