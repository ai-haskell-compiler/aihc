{-# LANGUAGE NoImplicitPrelude #-}
{- | Noise and random processes. -}
module Synthesizer.Plain.Noise where

import qualified Synthesizer.Plain.Signal as Sig

import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring

import System.Random (Random, RandomGen, randomRs, mkStdGen, )

import Data.List.HT (sliceVertical, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{-|
Deterministic white noise, uniformly distributed between -1 and 1.
That is, variance is 1\/3.
-}
white :: (Ring.C y, Random y) =>
   Sig.T y
white = whiteGen (mkStdGen 12354)

whiteGen :: (Ring.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteGen = randomRs (-1,1)

{- |
Approximates normal distribution with variance 1
by a quadratic B-spline distribution.
-}
whiteQuadraticBSplineGen :: (Ring.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteQuadraticBSplineGen =
   map sum . sliceVertical 3 . randomRs (-1,1)


randomPeeks :: (RealRing.C y, Random y) =>
      Sig.T y    {- ^ momentary densities, @p@ means that there is about one peak
                      in the time range of @1\/p@ samples -}
   -> Sig.T Bool {- ^ Every occurence of 'True' represents a peak. -}
randomPeeks =
   randomPeeksGen (mkStdGen 876)

randomPeeksGen :: (RealRing.C y, Random y, RandomGen g) =>
      g
   -> Sig.T y
   -> Sig.T Bool
randomPeeksGen =
   zipWith (<) . randomRs (0,1)
