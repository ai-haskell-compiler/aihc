{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

-}
module Synthesizer.Dimensional.RateAmplitude.Noise
  (white,    whiteBandEnergy,    randomPeeks,
   whiteGen, whiteBandEnergyGen, randomPeeksGen,
   ) where


import qualified Synthesizer.State.NoiseCustom as Noise
import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.RandomKnuth as Knuth

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Rate.Dirac as Dirac
import qualified Synthesizer.Dimensional.Process as Proc

import Synthesizer.Dimensional.Process (($#), )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
import Number.DimensionTerm ((&*&))

import qualified Algebra.Algebraic          as Algebraic
import qualified Algebra.Field              as Field

import System.Random (Random, RandomGen, mkStdGen)

import NumericPrelude.Numeric
import NumericPrelude.Base as P



{-# INLINE white #-}
{- The Field.C constraint could be replaced by Ring.C
   if Noise instead of faster NoiseCustom would be used -}
white :: (Field.C yv, Random yv, Algebraic.C q, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) q
          {-^ width of the frequency band -}
   -> DN.T v q
          {-^ volume caused by the given frequency band -}
   -> Proc.T s u q (SigA.R s v q yv)
          {-^ noise -}
white =
   -- FIXME: there was a bug in GHC-6.4's standard random generator where genRange returned minBound::Int as lower bound but actually generated numbers were always positive
   -- this is fixed in GHC-6.6 and thus the standard generator can be used
   whiteGen (Knuth.cons 6746)
--   whiteGen (mkStdGen 6746)

{-# INLINE whiteGen #-}
whiteGen ::
   (Field.C yv, Random yv, RandomGen g, Algebraic.C q, Dim.C u, Dim.C v) =>
      g   {-^ random generator, can be used to choose a seed -}
   -> DN.T (Dim.Recip u) q
          {-^ width of the frequency band -}
   -> DN.T v q
          {-^ volume caused by the given frequency band -}
   -> Proc.T s u q (SigA.R s v q yv)
          {-^ noise -}
whiteGen gen bandWidth volume =
   flip fmap (Proc.toFrequencyScalar bandWidth) $ \bw ->
   SigA.fromBody
      (DN.scale (sqrt $ 3 / bw) volume)
      (Noise.whiteGen gen)


{-# INLINE whiteBandEnergy #-}
whiteBandEnergy :: (Field.C yv, Random yv, Algebraic.C q, Dim.C u, Dim.C v) =>
      DN.T (Dim.Mul u (Dim.Sqr v)) q
          {-^ energy per frequency band -}
   -> Proc.T s u q (SigA.R s v q yv)
          {-^ noise -}
whiteBandEnergy = whiteBandEnergyGen (mkStdGen 6746)

{-# INLINE whiteBandEnergyGen #-}
whiteBandEnergyGen ::
   (Field.C yv, Random yv, RandomGen g, Algebraic.C q, Dim.C u, Dim.C v) =>
      g   {-^ random generator, can be used to choose a seed -}
   -> DN.T (Dim.Mul u (Dim.Sqr v)) q
          {-^ energy per frequency band -}
   -> Proc.T s u q (SigA.R s v q yv)
          {-^ noise -}
whiteBandEnergyGen gen energy =
   flip fmap Proc.getSampleRate $ \rate ->
   SigA.fromBody
      (DN.sqrt $ DN.scale 3 $
       DN.rewriteDimension
          (Dim.identityLeft . Dim.applyLeftMul Dim.cancelLeft .
           Dim.associateLeft) $
       rate &*& energy)
      (Noise.whiteGen gen)


{-
The Field.C q constraint could be lifted to Ring.C
if we would use direct division instead of toFrequencyScalar.
-}
{-# INLINE randomPeeks #-}
randomPeeks ::
   (Field.C q, Random q, Ord q, Dim.C u) =>
    Proc.T s u q (
       SigA.R s (Dim.Recip u) q q
          {- v instantaneous densities (frequency),
               @p@ means that there is about one peak
               in the time range of @1\/p@. -}
    -> SigA.R s (Dim.Recip u) q q)
          {- ^ Every occurrence is represented by a peak of area 1.
               If you smooth the input and the output signal to the same degree
               they should be rather similar. -}
randomPeeks =
   randomPeeksGen (mkStdGen 876)


{-# INLINE randomPeeksGen #-}
randomPeeksGen ::
   (Field.C q, Random q, Ord q, Dim.C u,
    RandomGen g) =>
       g  {- ^ random generator, can be used to choose a seed -}
    -> Proc.T s u q (
         SigA.R s (Dim.Recip u) q q
          {- v momentary densities (frequency),
               @p@ means that there is about one peak
               in the time range of @1\/p@. -}
      -> SigA.R s (Dim.Recip u) q q)
          {- ^ Every occurrence is represented by a peak of area 1. -}
randomPeeksGen g =
   Proc.withParam $ \ dens ->
      do freq <- Proc.toFrequencyScalar (SigA.actualAmplitude dens)
         Dirac.toAmplitudeSignal $#
            (Dirac.Cons $
             Sig.zipWith (<)
                (Noise.randomRs (0, recip freq) g)
                (SigA.body dens))
