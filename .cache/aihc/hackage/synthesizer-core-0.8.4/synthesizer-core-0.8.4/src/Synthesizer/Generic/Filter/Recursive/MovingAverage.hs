{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

-}
module Synthesizer.Generic.Filter.Recursive.MovingAverage (
   sumsStaticInt,
   modulatedFrac,
   ) where

import qualified Synthesizer.Generic.Signal  as SigG

import qualified Synthesizer.Generic.Filter.Recursive.Integration as Integration
import qualified Synthesizer.Generic.Filter.Delay as Delay

import qualified Synthesizer.State.Signal as SigS

import Data.Function.HT (nest, )

import qualified Algebra.Module                as Module
import qualified Algebra.RealField             as RealField
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Like 'Synthesizer.Generic.Filter.NonRecursive.sums' but in a recursive form.
This needs only linear time (independent of the window size)
but may accumulate rounding errors.

@
ys = xs * (1,0,0,0,-1) \/ (1,-1)
ys * (1,-1) = xs * (1,0,0,0,-1)
ys = xs * (1,0,0,0,-1) + ys * (0,1)
@
-}
{-# INLINE sumsStaticInt #-}
sumsStaticInt :: (Additive.C v, SigG.Write sig v) =>
   Int -> sig v -> sig v
sumsStaticInt n xs =
   Integration.run (sub xs (Delay.staticPos n xs))


{-# INLINE sub #-}
sub :: (Additive.C v, SigG.Transform sig v) =>
   sig v -> sig v -> sig v
sub xs ys =
   SigG.mix xs (SigG.map Additive.negate ys)


{-
Sum of a part of a vector with negative sign for reverse order.
It adds from @from@ (inclusively) to @to@ (exclusively),
that is, it sums up @abs (to-from)@ values

{-# INLINE sumFromTo #-}
sumFromTo :: (Additive.C v) => Int -> Int -> sig v -> v
sumFromTo from to =
   if from <= to
     then          Sig.sum . Sig.take (to-from) . Sig.drop from
     else negate . Sig.sum . Sig.take (from-to) . Sig.drop to
-}

{-# INLINE sumFromToFrac #-}
sumFromToFrac ::
   (RealField.C a, Module.C a v, SigG.Transform sig v) =>
   a -> a -> sig v -> v
sumFromToFrac from to xs =
   let (fromInt, fromFrac) = splitFraction from
       (toInt,   toFrac)   = splitFraction to
   in  case compare fromInt toInt of
          EQ -> (to-from) *> index zero fromInt xs
          LT ->
            (addNext ((1-fromFrac) *>) $
             nest (toInt-fromInt-1) (addNext id) $
             addNext (toFrac *>) $
             const)
            zero (SigG.drop fromInt xs)
          GT ->
            (addNext ((1-toFrac) *>) $
             nest (fromInt-toInt-1) (addNext id) $
             addNext (fromFrac *>) $
             const)
            zero (SigG.drop toInt xs)


{-# INLINE index #-}
index ::
   (SigG.Transform sig y) =>
   y -> Int -> sig y -> y
index deflt n =
   maybe deflt fst . SigG.viewL . SigG.drop n


{-# INLINE addNext #-}
addNext ::
   (Additive.C v, SigG.Transform sig a) =>
   (a -> v) -> (v -> sig a -> v) -> v -> sig a -> v
addNext f next s =
   SigG.switchL s
      (\y ys -> next (s + f y) ys)


{- |
@sig a@ must contain only non-negative elements.
-}
{-# INLINE sumDiffsModulated #-}
sumDiffsModulated ::
   (RealField.C a, Module.C a v, SigG.Transform sig a, SigG.Transform sig v) =>
   a -> sig a -> sig v -> sig v
sumDiffsModulated d ds =
   maybe (error "MovingAverage: signal must be non-empty because we prepended a zero before") fst .
   SigG.viewR .
   -- prevent negative d's since 'drop' cannot restore past values
   zipRangesWithTails sumFromToFrac
      (SigG.cons (d+1) ds) (SigG.map (1+) ds) .
   SigG.cons zero

{-
   zipRangesWithTails sumFromToFrac
      (SigG.cons d (SigG.map (subtract 1) ds)) ds
-}

zipRangesWithTails ::
   (SigG.Transform sig a, SigG.Transform sig v) =>
   (a -> a -> sig v -> v) -> sig a -> sig a -> sig v -> sig v
zipRangesWithTails f tls tus xs =
   SigG.zipWithState
      (\(tl,suffix) tu -> f tl tu suffix)
      (SigS.zip (SigG.toState tls) (SigG.tails xs))
      tus

{-
{-# INLINE sumsModulated #-}
sumsModulated :: (RealField.C a, Module.C a v) =>
   Int -> sig a -> sig v -> sig v
sumsModulated maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       posXs = sumDiffsModulated 0 ds xs
       negXs = sumDiffsModulated maxD (SigG.map (maxD-) ds) (Delay.static maxDInt xs)
   in  Integration.run (sub posXs negXs)
-}

{- |
Shift sampling points by a half sample period
in order to preserve signals for window widths below 1.
-}
{-# INLINE sumsModulatedHalf #-}
sumsModulatedHalf ::
   (RealField.C a, Module.C a v, SigG.Transform sig a, SigG.Write sig v) =>
   Int -> sig a -> sig v -> sig v
sumsModulatedHalf maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       d0    = maxD+0.5
       delXs = Delay.staticPos maxDInt xs
       posXs = sumDiffsModulated d0 (SigG.map (d0+) ds) delXs
       negXs = sumDiffsModulated d0 (SigG.map (d0-) ds) delXs
   in  Integration.run (sub posXs negXs)

{-# INLINE modulatedFrac #-}
modulatedFrac ::
   (RealField.C a, Module.C a v, SigG.Transform sig a, SigG.Write sig v) =>
   Int -> sig a -> sig v -> sig v
modulatedFrac maxDInt ds xs =
   SigG.zipWith (\d y -> recip (2*d) *> y) ds $
   sumsModulatedHalf maxDInt ds xs
