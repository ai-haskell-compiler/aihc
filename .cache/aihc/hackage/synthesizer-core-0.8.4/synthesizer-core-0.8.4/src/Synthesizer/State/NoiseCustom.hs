{-# LANGUAGE NoImplicitPrelude #-}
{- |
Noise and random processes.
This uses a fast reimplementation of 'System.Random.randomR'
since the standard function seems not to be inlined (at least in GHC-6.8.2).
-}
module Synthesizer.State.NoiseCustom where

import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.RandomKnuth as Knuth
import qualified System.Random as Rnd
import System.Random (Random, RandomGen, )

import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field

import NumericPrelude.Numeric
import NumericPrelude.Base


{-|
Deterministic white noise, uniformly distributed between -1 and 1.
That is, variance is 1\/3.
-}
{-# INLINE white #-}
white :: (Field.C y, Random y) =>
   Sig.T y
white = whiteGen (Knuth.cons 12354)

{-# INLINE whiteGen #-}
whiteGen ::
   (Field.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteGen = randomRs (-1,1)


{- |
Approximates normal distribution with variance 1
by a quadratic B-spline distribution.
-}
{-# INLINE whiteQuadraticBSplineGen #-}
whiteQuadraticBSplineGen ::
   (Field.C y, Random y, RandomGen g) =>
   g -> Sig.T y
whiteQuadraticBSplineGen g =
   let (g0,gr) = Rnd.split g
       (g1,g2) = Rnd.split gr
   in  whiteGen g0 `Sig.mix`
       whiteGen g1 `Sig.mix`
       whiteGen g2


{-# INLINE randomPeeks #-}
randomPeeks :: (RealField.C y, Random y) =>
      Sig.T y    {- ^ momentary densities, @p@ means that there is about one peak
                      in the time range of @1\/p@ samples -}
   -> Sig.T Bool {- ^ Every occurence of 'True' represents a peak. -}
randomPeeks =
   randomPeeksGen (Knuth.cons 876)

{-# INLINE randomPeeksGen #-}
randomPeeksGen :: (RealField.C y, Random y, RandomGen g) =>
      g
   -> Sig.T y
   -> Sig.T Bool
randomPeeksGen =
   Sig.zipWith (<) . randomRs (0,1)


{-# INLINE randomRs #-}
randomRs ::
   (Field.C y, Random y, RandomGen g) =>
   (y,y) -> g -> Sig.T y
randomRs bnd = Sig.unfoldR (Just . randomR bnd)

{-# INLINE randomR #-}
randomR ::
   (RandomGen g, Field.C y) =>
   (y, y) -> g -> (y, g)
randomR (lower,upper) g0 =
   let (n,g1) = Rnd.next g0
       (l,u) = Rnd.genRange g0
       nd = fromIntegral n
       ld = fromIntegral l
       ud = fromIntegral u
       x01 = (nd-ld)/(ud-ld)
   in  ((1-x01)*lower + x01*upper, g1)
