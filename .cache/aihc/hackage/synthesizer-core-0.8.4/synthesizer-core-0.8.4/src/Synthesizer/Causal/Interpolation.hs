{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Interpolation (
   Interpolation.T,

   relative,
   relativeZeroPad,
   relativeConstantPad,
   relativeCyclicPad,
   relativeExtrapolationPad,
   relativeZeroPadConstant,
   relativeZeroPadLinear,
   relativeZeroPadCubic,
   ) where

import qualified Synthesizer.Interpolation.Module as IpExample
import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.State.Interpolation as InterpolationS

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.State.Signal   as Sig

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField
import qualified Algebra.RealRing  as RealRing
import qualified Algebra.Additive  as Additive


import NumericPrelude.Numeric
import NumericPrelude.Base


{-* Interpolation at multiple nodes with various padding methods -}

{- | All values of frequency control must be non-negative. -}
{-# INLINE relative #-}
relative :: (RealRing.C t) =>
   Interpolation.T t y -> t -> Sig.T y -> Causal.T t y
relative ip phase0 x0 =
   Causal.crochetL
      (\freq pos ->
          let (phase,x) = InterpolationS.skip ip pos
          in  Just (Interpolation.func ip phase x, (phase+freq,x)))
      (phase0,x0)


{-# INLINE relativeZeroPad #-}
relativeZeroPad :: (RealRing.C t) =>
   y -> Interpolation.T t y -> t -> Sig.T y -> Causal.T t y
relativeZeroPad z ip phase x =
   InterpolationS.zeroPad relative z ip phase x

{-# INLINE relativeConstantPad #-}
relativeConstantPad :: (RealRing.C t) =>
   Interpolation.T t y -> t -> Sig.T y -> Causal.T t y
relativeConstantPad ip phase x =
   InterpolationS.constantPad relative ip phase x

{-# INLINE relativeCyclicPad #-}
relativeCyclicPad :: (RealRing.C t) =>
   Interpolation.T t y -> t -> Sig.T y -> Causal.T t y
relativeCyclicPad ip phase x =
   InterpolationS.cyclicPad relative ip phase x

{- |
The extrapolation may miss some of the first and some of the last points
-}
{-# INLINE relativeExtrapolationPad #-}
relativeExtrapolationPad :: (RealRing.C t) =>
   Interpolation.T t y -> t -> Sig.T y -> Causal.T t y
relativeExtrapolationPad ip phase x =
   InterpolationS.extrapolationPad relative ip phase x
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}

{-* All-in-one interpolation functions -}

{-# INLINE relativeZeroPadConstant #-}
relativeZeroPadConstant ::
   (RealRing.C t, Additive.C y) =>
   t -> Sig.T y -> Causal.T t y
relativeZeroPadConstant =
   relativeZeroPad zero IpExample.constant

{-# INLINE relativeZeroPadLinear #-}
relativeZeroPadLinear ::
   (RealRing.C t, Module.C t y) =>
   t -> Sig.T y -> Causal.T t y
relativeZeroPadLinear =
   relativeZeroPad zero IpExample.linear

{-# INLINE relativeZeroPadCubic #-}
relativeZeroPadCubic ::
   (RealField.C t, Module.C t y) =>
   t -> Sig.T y -> Causal.T t y
relativeZeroPadCubic =
   relativeZeroPad zero IpExample.cubic

