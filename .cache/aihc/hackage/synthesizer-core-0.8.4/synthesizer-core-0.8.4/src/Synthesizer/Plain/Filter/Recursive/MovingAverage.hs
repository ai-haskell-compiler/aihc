{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Filter.Recursive.MovingAverage (
   sumsStaticInt,
   modulatedFrac,
   ) where

import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Filter.Recursive.Integration as Integration

import Synthesizer.Plain.Filter.NonRecursive (delay, )

import qualified Algebra.Module                as Module
import qualified Algebra.RealField             as RealField
import qualified Algebra.Additive              as Additive

import Control.Monad.Fix (fix)
import Data.List (tails)

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Like 'Synthesizer.Plain.Filter.NonRecursive.sums' but in a recursive form.
This needs only linear time (independent of the window size)
but may accumulate rounding errors.

@
ys = xs * (1,0,0,0,-1) \/ (1,-1)
ys * (1,-1) = xs * (1,0,0,0,-1)
ys = xs * (1,0,0,0,-1) + ys * (0,1)
@
-}
sumsStaticInt :: (Additive.C v) => Int -> Sig.T v -> Sig.T v
sumsStaticInt n xs =
   fix (\ys -> let (xs0,xs1) = splitAt n xs
               in  (xs0 ++ (xs1-xs)) + (zero:ys))

{-
staticInt :: (Module.C a v, Additive.C v) => Int -> Sig.T v -> Sig.T v
staticInt n xs =
-}


{-
Sum of a part of a vector with negative sign for reverse order.
It adds from @from@ (inclusively) to @to@ (exclusively),
that is, it sums up @abs (to-from)@ values
-}
_sumFromTo :: (Additive.C v) => Int -> Int -> Sig.T v -> v
_sumFromTo from to =
   if from <= to
     then          sum . take (to-from) . drop from
     else negate . sum . take (from-to) . drop to

{-
It would be a nice approach to interpolate not just linearly at the borders
but in a way that the cut-off frequency is perfectly suppressed.
However suppression depends on the phase shift of the wave.
Actually, we could use a complex factor, but does this help?
-}
sumFromToFrac :: (RealField.C a, Module.C a v) => a -> a -> Sig.T v -> v
sumFromToFrac from to xs =
   let (fromInt, fromFrac) = splitFraction from
       (toInt,   toFrac)   = splitFraction to
   in  case compare fromInt toInt of
          EQ -> (to-from) *> (xs !! fromInt)
          LT ->
            sum $
            zipWith id
               (((1-fromFrac) *>) :
                replicate (toInt-fromInt-1) id ++
                (toFrac *>) :
                []) $
            drop fromInt xs
          GT ->
            negate $ sum $
            zipWith id
               (((1-toFrac) *>) :
                replicate (fromInt-toInt-1) id ++
                (fromFrac *>) :
                []) $
            drop toInt xs



{- |
Sig.T a must contain only non-negative elements.
-}
sumDiffsModulated :: (RealField.C a, Module.C a v) =>
   a -> Sig.T a -> Sig.T v -> Sig.T v
sumDiffsModulated d ds =
   -- prevent negative d's since 'drop' cannot restore past values
   zipWith3 sumFromToFrac ((d+1) : ds) (map (1+) ds) .
   init . init . tails . (zero:)
{-
   zipWith3 sumFromToFrac (d : map (subtract 1) ds) ds .
   init . tails
-}

_sumsModulated :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
_sumsModulated maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       posXs = sumDiffsModulated 0 ds xs
       negXs = sumDiffsModulated maxD (map (maxD-) ds) (delay maxDInt xs)
   in  Integration.run (posXs - negXs)

{- |
Shift sampling points by a half sample period
in order to preserve signals for window widths below 1.
-}
sumsModulatedHalf :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
sumsModulatedHalf maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       d0    = maxD+0.5
       delXs = delay maxDInt xs
       posXs = sumDiffsModulated d0 (map (d0+) ds) delXs
       negXs = sumDiffsModulated d0 (map (d0-) ds) delXs
   in  Integration.run (posXs - negXs)

{-
*Synthesizer.Plain.Filter.NonRecursive> movingAverageModulated 10 (replicate 10 (3::Double) ++ [1.1,2.2,2.6,0.7,0.1,0.1]) (repeat (1::Double))
[0.5,0.6666666666666666,0.8333333333333333,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.9999999999999999,1.0,0.9999999999999998,0.999999999999999,0.9999999999999942,0.9999999999999942]
-}

modulatedFrac :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
modulatedFrac maxDInt ds xs =
   zipWith (\d y -> recip (2*d) *> y) ds $
   sumsModulatedHalf maxDInt ds xs

