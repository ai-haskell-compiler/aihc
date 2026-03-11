{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{- | Noise and random processes. -}
module Synthesizer.Generic.Noise where

import qualified Synthesizer.State.Noise as Noise

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as SigS

import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring

import System.Random (Random, RandomGen, randomR, mkStdGen, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{-|
Deterministic white noise, uniformly distributed between -1 and 1.
That is, variance is 1\/3.
-}
white ::
   (Ring.C y, Random y, SigG.Write sig y) =>
   SigG.LazySize -> sig y
white size =
   SigG.fromState size $ Noise.white

whiteGen ::
   (Ring.C y, Random y, RandomGen g, SigG.Write sig y) =>
   SigG.LazySize -> g -> sig y
whiteGen size =
   SigG.fromState size . Noise.whiteGen


{- |
Approximates normal distribution with variance 1
by a quadratic B-spline distribution.
-}
whiteQuadraticBSplineGen ::
   (Ring.C y, Random y, RandomGen g, SigG.Write sig y) =>
   SigG.LazySize -> g -> sig y
whiteQuadraticBSplineGen size =
   SigG.fromState size . Noise.whiteQuadraticBSplineGen


randomPeeks ::
   (RealRing.C y, Random y, SigG.Transform sig y, SigG.Transform sig Bool) =>
      sig y    {- ^ momentary densities, @p@ means that there is about one peak
                      in the time range of @1\/p@ samples -}
   -> sig Bool {- ^ Every occurence of 'True' represents a peak. -}
randomPeeks =
   randomPeeksGen (mkStdGen 876)

randomPeeksGen ::
   (RealRing.C y, Random y, RandomGen g, SigG.Transform sig y, SigG.Transform sig Bool) =>
      g
   -> sig y
   -> sig Bool
randomPeeksGen =
   SigG.zipWithState (<) . SigS.unfoldR (Just . randomR (0,1))
