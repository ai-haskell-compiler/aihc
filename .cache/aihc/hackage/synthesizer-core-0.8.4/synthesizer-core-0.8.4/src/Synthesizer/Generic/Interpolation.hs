{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Generic.Interpolation (
   T, func, offset, number,
   zeroPad, constantPad, cyclicPad, extrapolationPad,
   single,
   multiRelative,
   multiRelativeZeroPad, multiRelativeConstantPad,
   multiRelativeCyclicPad, multiRelativeExtrapolationPad,
   multiRelativeZeroPadConstant, multiRelativeZeroPadLinear,
   multiRelativeZeroPadCubic,
   ) where

import qualified Synthesizer.Interpolation as Interpolation
import Synthesizer.Interpolation (T, offset, number, )
import Synthesizer.Interpolation.Module (constant, linear, cubic, )

import qualified Synthesizer.Generic.Signal  as SigG
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNR

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField
import qualified Algebra.RealRing  as RealRing
import qualified Algebra.Additive  as Additive

import Data.Maybe (fromMaybe, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{-* Interpolation with various padding methods -}

{-# INLINE zeroPad #-}
zeroPad :: (RealRing.C t, SigG.Write sig y) =>
   (T t y -> t -> sig y -> a) ->
   y -> T t y -> t -> sig y -> a
zeroPad interpolate z ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (FiltNR.delayPad z (offset ip - phInt)
              (SigG.append x (SigG.repeat SigG.defaultLazySize z)))

{-# INLINE constantPad #-}
constantPad :: (RealRing.C t, SigG.Write sig y) =>
   (T t y -> t -> sig y -> a) ->
   T t y -> t -> sig y -> a
constantPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
       xPad =
          do (xFirst,_) <- SigG.viewL x
             return (FiltNR.delayPad xFirst
                (offset ip - phInt) (SigG.extendConstant SigG.defaultLazySize x))
   in  interpolate ip phFrac
          (fromMaybe SigG.empty xPad)


{- |
Only for finite input signals.
-}
{-# INLINE cyclicPad #-}
cyclicPad :: (RealRing.C t, SigG.Transform sig y) =>
   (T t y -> t -> sig y -> a) ->
   T t y -> t -> sig y -> a
cyclicPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (SigG.drop (mod (phInt - offset ip) (SigG.length x)) (SigG.cycle x))

{- |
The extrapolation may miss some of the first and some of the last points
-}
{-# INLINE extrapolationPad #-}
extrapolationPad :: (RealRing.C t, SigG.Transform sig y) =>
   (T t y -> t -> sig y -> a) ->
   T t y -> t -> sig y -> a
extrapolationPad interpolate ip phase =
   interpolate ip (phase - fromIntegral (offset ip))
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}


{-* Interpolation of multiple values with various padding methods -}

func :: (SigG.Read sig y) =>
   T t y -> t -> sig y -> y
func ip phase =
   Interpolation.func ip phase . SigG.toState

{-# INLINE skip #-}
skip :: (RealRing.C t, SigG.Transform sig y) =>
   T t y -> (t, sig y) -> (t, sig y)
skip ip (phase0, x0) =
   let (n, frac) = splitFraction phase0
       (m, x1) = SigG.dropMarginRem (number ip) n x0
   in  (fromIntegral m + frac, x1)

{-# INLINE single #-}
single :: (RealRing.C t, SigG.Transform sig y) =>
   T t y -> t -> sig y -> y
single ip phase0 x0 =
   uncurry (func ip) $ skip ip (phase0, x0)
--   curry (uncurry (func ip) . skip ip)
{-
GNUPlot.plotFunc [] (GNUPlot.linearScale 1000 (0,2)) (\t -> single linear (t::Double) [0,4,1::Double])
-}


{-* Interpolation of multiple values with various padding methods -}

{- | All values of frequency control must be non-negative. -}
{-# INLINE multiRelative #-}
multiRelative ::
   (RealRing.C t, SigG.Transform sig t, SigG.Transform sig y) =>
   T t y -> t -> sig y -> sig t -> sig y
multiRelative ip phase0 x0 =
   SigG.crochetL
      (\freq pos ->
          let (phase,x) = skip ip pos
          in  Just (func ip phase x, (phase+freq,x)))
      (phase0,x0)


{-# INLINE multiRelativeZeroPad #-}
multiRelativeZeroPad ::
   (RealRing.C t, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   y -> T t y -> t -> sig t -> sig y -> sig y
multiRelativeZeroPad z ip phase fs x =
   zeroPad multiRelative z ip phase x fs

{-# INLINE multiRelativeConstantPad #-}
multiRelativeConstantPad ::
   (RealRing.C t, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   T t y -> t -> sig t -> sig y -> sig y
multiRelativeConstantPad ip phase fs x =
   constantPad multiRelative ip phase x fs

{-# INLINE multiRelativeCyclicPad #-}
multiRelativeCyclicPad ::
   (RealRing.C t, SigG.Transform sig t, SigG.Transform sig y) =>
   T t y -> t -> sig t -> sig y -> sig y
multiRelativeCyclicPad ip phase fs x =
   cyclicPad multiRelative ip phase x fs

{- |
The extrapolation may miss some of the first and some of the last points
-}
{-# INLINE multiRelativeExtrapolationPad #-}
multiRelativeExtrapolationPad ::
   (RealRing.C t, SigG.Transform sig t, SigG.Transform sig y) =>
   T t y -> t -> sig t -> sig y -> sig y
multiRelativeExtrapolationPad ip phase fs x =
   extrapolationPad multiRelative ip phase x fs
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}

{-* All-in-one interpolation functions -}

{-# INLINE multiRelativeZeroPadConstant #-}
multiRelativeZeroPadConstant ::
   (RealRing.C t, Additive.C y, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   t -> sig t -> sig y -> sig y
multiRelativeZeroPadConstant =
   multiRelativeZeroPad zero constant

{-# INLINE multiRelativeZeroPadLinear #-}
multiRelativeZeroPadLinear ::
   (RealRing.C t, Module.C t y, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   t -> sig t -> sig y -> sig y
multiRelativeZeroPadLinear =
   multiRelativeZeroPad zero linear

{-# INLINE multiRelativeZeroPadCubic #-}
multiRelativeZeroPadCubic ::
   (RealField.C t, Module.C t y, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   t -> sig t -> sig y -> sig y
multiRelativeZeroPadCubic =
   multiRelativeZeroPad zero cubic
