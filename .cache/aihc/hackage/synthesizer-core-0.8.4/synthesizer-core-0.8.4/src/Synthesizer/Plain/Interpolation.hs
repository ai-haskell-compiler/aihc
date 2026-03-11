{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Interpolation (
   T, func, offset, number,
   zeroPad, constantPad, cyclicPad, extrapolationPad,
   single,
   multiRelative,
   multiRelativeZeroPad, multiRelativeConstantPad,
   multiRelativeCyclicPad, multiRelativeExtrapolationPad,
   multiRelativeZeroPadConstant, multiRelativeZeroPadLinear,
   multiRelativeZeroPadCubic,

   constant, linear, cubic,
   piecewise, function,

   Interpolation.Margin, Interpolation.margin,

   -- for testing
   singleRec,
   ) where

import qualified Synthesizer.Interpolation as Interpolation
import Synthesizer.Interpolation (T, offset, number, )
import Synthesizer.Interpolation.Module
          (constant, linear, cubic, piecewise, function, )

import qualified Synthesizer.State.Signal       as SigS

import qualified Synthesizer.Plain.Signal  as Sig
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR

import Control.Monad (guard, )

import qualified Data.List.HT as ListHT
import Data.Maybe (fromMaybe)

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField
import qualified Algebra.RealRing  as RealRing
import qualified Algebra.Ring      as Ring
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{-* Interpolation with various padding methods -}

zeroPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   y -> T t y -> t -> Sig.T y -> a
zeroPad interpolate z ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (FiltNR.delayPad z (offset ip - phInt) (x ++ repeat z))

constantPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
constantPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
       xPad =
          do (xFirst,_) <- ListHT.viewL x
             (xBody,xLast) <- ListHT.viewR x
             return (FiltNR.delayPad xFirst (offset ip - phInt) (xBody ++ repeat xLast))
   in  interpolate ip phFrac
          (fromMaybe [] xPad)


{- |
Only for finite input signals.
-}
cyclicPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
cyclicPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (drop (mod (phInt - offset ip) (length x)) (cycle x))

{- |
The extrapolation may miss some of the first and some of the last points
-}
extrapolationPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
extrapolationPad interpolate ip phase =
   interpolate ip (phase - fromIntegral (offset ip))
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}


{-* Interpolation of multiple values with various padding methods -}

func ::
   T t y -> t -> Sig.T y -> y
func ip phase =
   Interpolation.func ip phase . SigS.fromList

skip :: (RealRing.C t) =>
   T t y -> (t, Sig.T y) -> (t, Sig.T y)
skip ip (phase0, x0) =
   let (n, frac) = splitFraction phase0
       (m, x1) = Sig.dropMarginRem (number ip) n x0
   in  (fromIntegral m + frac, x1)

single :: (RealRing.C t) =>
   T t y -> t -> Sig.T y -> y
single ip phase0 x0 =
   uncurry (func ip) $ skip ip (phase0, x0)
--   curry (uncurry (func ip) . skip ip)
{-
GNUPlot.plotFunc [] (GNUPlot.linearScale 1000 (0,2)) (\t -> single linear (t::Double) [0,4,1::Double])
-}

-- | alternative implementation of 'single'
singleRec :: (Ord t, Ring.C t) =>
   T t y -> t -> Sig.T y -> y
singleRec ip phase x =
   -- check if we are leaving the current interval
   maybe
      (func ip phase x)
      (singleRec ip (phase - 1))
      (do (_,xs) <- ListHT.viewL x
          guard (phase >= 1 && Sig.lengthAtLeast (number ip) xs)
          return xs)


{-* Interpolation of multiple values with various padding methods -}

{- | All values of frequency control must be non-negative. -}
multiRelative :: (RealRing.C t) =>
   T t y -> t -> Sig.T y -> Sig.T t -> Sig.T y
multiRelative ip phase0 x0 =
   map (uncurry (func ip)) .
   scanl
      (\(phase,x) freq -> skip ip (phase + freq, x))
      (skip ip (phase0,x0))


multiRelativeZeroPad :: (RealRing.C t) =>
   y -> T t y -> t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeZeroPad z ip phase fs x =
   zeroPad multiRelative z ip phase x fs

multiRelativeConstantPad :: (RealRing.C t) =>
   T t y -> t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeConstantPad ip phase fs x =
   constantPad multiRelative ip phase x fs

multiRelativeCyclicPad :: (RealRing.C t) =>
   T t y -> t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeCyclicPad ip phase fs x =
   cyclicPad multiRelative ip phase x fs

{- |
The extrapolation may miss some of the first and some of the last points
-}
multiRelativeExtrapolationPad :: (RealRing.C t) =>
   T t y -> t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeExtrapolationPad ip phase fs x =
   extrapolationPad multiRelative ip phase x fs
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}

{-* All-in-one interpolation functions -}

multiRelativeZeroPadConstant ::
   (RealRing.C t, Additive.C y) => t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeZeroPadConstant = multiRelativeZeroPad zero constant

multiRelativeZeroPadLinear ::
   (RealRing.C t, Module.C t y) => t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeZeroPadLinear = multiRelativeZeroPad zero linear

multiRelativeZeroPadCubic ::
   (RealField.C t, Module.C t y) => t -> Sig.T t -> Sig.T y -> Sig.T y
multiRelativeZeroPadCubic = multiRelativeZeroPad zero cubic
