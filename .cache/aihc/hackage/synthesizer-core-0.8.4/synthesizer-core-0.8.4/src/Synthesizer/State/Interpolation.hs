{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.State.Interpolation (
   zeroPad,
   constantPad,
   cyclicPad,
   extrapolationPad,

   skip,
   single,

   -- imported in State.Filter.Delay
   delayPad,
   ) where

import Synthesizer.Interpolation (T, offset, number, func, )

import qualified Synthesizer.State.Signal  as Sig

import Data.Maybe (fromMaybe)

import qualified Algebra.RealRing  as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base


{-* Interpolation with various padding methods -}

{-# INLINE zeroPad #-}
zeroPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   y -> T t y -> t -> Sig.T y -> a
zeroPad interpolate z ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (delayPad z (offset ip - phInt) (Sig.append x (Sig.repeat z)))

{-# INLINE constantPad #-}
constantPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
constantPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
       xPad =
          do (xFirst,_) <- Sig.viewL x
             return (delayPad xFirst (offset ip - phInt) (Sig.extendConstant x))
   in  interpolate ip phFrac
          (fromMaybe Sig.empty xPad)


{- |
Only for finite input signals.
-}
{-# INLINE cyclicPad #-}
cyclicPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
cyclicPad interpolate ip phase x =
   let (phInt, phFrac) = splitFraction phase
   in  interpolate ip phFrac
          (Sig.drop (mod (phInt - offset ip) (Sig.length x)) (Sig.cycle x))

{- |
The extrapolation may miss some of the first and some of the last points
-}
{-# INLINE extrapolationPad #-}
extrapolationPad :: (RealRing.C t) =>
   (T t y -> t -> Sig.T y -> a) ->
   T t y -> t -> Sig.T y -> a
extrapolationPad interpolate ip phase =
   interpolate ip (phase - fromIntegral (offset ip))
{-
  This example shows pikes, although there shouldn't be any:
   plotList (take 100 $ interpolate (Zero (0::Double)) ipCubic (-0.9::Double) (repeat 0.03) [1,0,1,0.8])
-}


{-* Helper methods for interpolation of multiple nodes -}

{-# INLINE skip #-}
skip :: (RealRing.C t) =>
   T t y -> (t, Sig.T y) -> (t, Sig.T y)
skip ip (phase0, x0) =
   let (n, frac) = splitFraction phase0
       (m, x1) = Sig.dropMarginRem (number ip) n x0
   in  (fromIntegral m + frac, x1)

{-# INLINE single #-}
single :: (RealRing.C t) =>
   T t y -> t -> Sig.T y -> y
single ip phase0 x0 =
   uncurry (func ip) $ skip ip (phase0, x0)
--   curry (uncurry (func ip) . skip ip)
{-
GNUPlot.plotFunc [] (GNUPlot.linearScale 1000 (0,2)) (\t -> single linear (t::Double) [0,4,1::Double])
-}


{-* Helper functions -}

{-# INLINE delayPad #-}
delayPad :: y -> Int -> Sig.T y -> Sig.T y
delayPad z n =
   if n<0
     then Sig.drop (negate n)
     else Sig.append (Sig.replicate n z)
