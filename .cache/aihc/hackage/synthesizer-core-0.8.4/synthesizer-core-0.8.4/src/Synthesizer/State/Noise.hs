{-# LANGUAGE NoImplicitPrelude #-}
{- | Noise and random processes. -}
module Synthesizer.State.Noise where

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring

import System.Random (Random, RandomGen, randomR, mkStdGen, )
import qualified System.Random as Rnd

import NumericPrelude.Numeric
import NumericPrelude.Base


{-|
Deterministic white noise, uniformly distributed between -1 and 1.
That is, variance is 1\/3.
-}
{-# INLINE white #-}
white :: (Ring.C y, Random y) =>
   Sig.T y
white = whiteGen (mkStdGen 12354)

{-# INLINE whiteGen #-}
whiteGen ::
   (Ring.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteGen = randomRs (-1,1)


{- |
Approximates normal distribution with variance 1
by a quadratic B-spline distribution.
-}
{-# INLINE whiteQuadraticBSplineGen #-}
whiteQuadraticBSplineGen ::
   (Ring.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteQuadraticBSplineGen g =
   let (g0,gr) = Rnd.split g
       (g1,g2) = Rnd.split gr
   in  whiteGen g0 `Sig.mix`
       whiteGen g1 `Sig.mix`
       whiteGen g2


{-# INLINE randomPeeks #-}
randomPeeks :: (RealRing.C y, Random y) =>
      Sig.T y    {- ^ momentary densities, @p@ means that there is about one peak
                      in the time range of @1\/p@ samples -}
   -> Sig.T Bool {- ^ Every occurence of 'True' represents a peak. -}
randomPeeks =
   randomPeeksGen (mkStdGen 876)

{-# INLINE randomPeeksGen #-}
randomPeeksGen :: (RealRing.C y, Random y, RandomGen g) =>
      g
   -> Sig.T y
   -> Sig.T Bool
randomPeeksGen =
   Sig.zipWith (<) . randomRs (0,1)



{-# INLINE randomRs #-}
randomRs ::
   (Ring.C y, Random y, RandomGen g) =>
   (y,y) -> g -> Sig.T y
randomRs bnd = Sig.unfoldR (Just . randomR bnd)
