{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Causal.Filter (
   -- * Non-recursive

   -- ** Amplification
   amplify,
   amplifyDimension,
   amplifyScalarDimension,
   negate,
   envelope,
   envelopeScalarDimension,
   envelopeVector,
   envelopeVectorDimension,

   -- ** Filter operators from calculus
   differentiate,

{-
   -- ** Smooth
   meanStatic,
   mean,

   -- ** Delay
   delay,
   phaseModulation,
   frequencyModulation,
   frequencyModulationDecoupled,
   phaser,
   phaserStereo,
-}


{-
   -- * Recursive

   -- ** Reverb
   comb,
   combProc,
-}

   -- ** Filter operators from calculus
   integrate,
) where

import qualified Synthesizer.Dimensional.Map.Filter as FiltM
import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((^<<), (&&&), )

import Synthesizer.Dimensional.Process (DimensionGradient, )

import qualified Synthesizer.State.Filter.Recursive.Integration as Integrate
-- import qualified Synthesizer.State.Filter.Recursive.MovingAverage as MA

-- import qualified Synthesizer.Generic.Filter.Recursive.Comb as Comb
-- import qualified Synthesizer.Dimensional.Causal.Displacement as DispC

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&), )

-- import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
-- import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
-- import qualified Algebra.VectorSpace    as VectorSpace
import qualified Algebra.Module         as Module

import NumericPrelude.Numeric hiding (negate)
import NumericPrelude.Base as P
import Prelude ()


{- | The amplification factor must be positive. -}
{-# INLINE amplify #-}
amplify :: (Module.C y amp) =>
   y ->
   Proc.T s u t (CausalD.Single s (Amp.Numeric amp) (Amp.Numeric amp) yv yv)
amplify volume =
   Proc.pure $ FiltM.amplify volume

{-# INLINE amplifyDimension #-}
amplifyDimension :: (Ring.C y, Dim.C u, Dim.C v0, Dim.C v1) =>
   DN.T v0 y ->
   Proc.T s u t
      (CausalD.Single s
          (Amp.Dimensional v1 y) (Amp.Dimensional (Dim.Mul v0 v1) y)
          yv yv)
amplifyDimension volume =
   Proc.pure $ FiltM.amplifyDimension volume

{-# INLINE amplifyScalarDimension #-}
amplifyScalarDimension :: (Ring.C y, Dim.C u, Dim.C v) =>
   DN.T v y ->
   Proc.T s u t
      (CausalD.Single s
          (Amp.Dimensional Dim.Scalar y) (Amp.Dimensional v y)
          yv yv)
amplifyScalarDimension volume =
   Proc.pure $ FiltM.amplifyScalarDimension volume


{-# INLINE negate #-}
negate :: (Additive.C (Sample.Displacement sample)) =>
   Proc.T s u t (CausalD.T s sample sample)
negate =
   Proc.pure $ FiltM.negate


{-# INLINE envelope #-}
envelope :: (Ring.C y) =>
   Proc.T s u t (CausalD.T s (Sample.Flat y, Sample.Numeric amp y) (Sample.Numeric amp y))
envelope =
   Proc.pure $ FiltM.envelope

{-# INLINE envelopeScalarDimension #-}
envelopeScalarDimension ::
   (Ring.C y, Dim.C u, Dim.C v) =>
   Proc.T s u t
      (CausalD.T s
          (Sample.Dimensional Dim.Scalar y y, Sample.Dimensional v y y)
          (Sample.Dimensional v y y))
envelopeScalarDimension =
   Proc.pure $ FiltM.envelopeScalarDimension

{-# INLINE envelopeVector #-}
envelopeVector :: (Module.C y (Sample.Displacement sample)) =>
   Proc.T s u t (CausalD.T s (Sample.Flat y, sample) sample)
envelopeVector =
   Proc.pure $ FiltM.envelopeVector

{-# INLINE envelopeVectorDimension #-}
envelopeVectorDimension ::
   (Module.C y0 yv, Ring.C y, Dim.C u, Dim.C v0, Dim.C v1) =>
   Proc.T s u t
      (CausalD.T s
          (Sample.Dimensional v0 y y0, Sample.Dimensional v1 y yv)
          (Sample.Dimensional (Dim.Mul v0 v1) y yv))
envelopeVectorDimension =
   Proc.pure $ FiltM.envelopeVectorDimension


{-# INLINE differentiate #-}
differentiate :: (Additive.C yv, Ring.C q, Dim.C u, Dim.C v) =>
   Proc.T s u q
      (CausalD.Single s
         (Amp.Dimensional v q) (Amp.Dimensional (DimensionGradient u v) q) yv yv)
differentiate =
   flip fmap Proc.getSampleRate $ \rate ->
      CausalD.consFlip $ \ (Amp.Numeric amp) ->
         (Amp.Numeric $ rate &*& amp,
          uncurry (-) ^<< Causal.id &&& Causal.consInit zero)
--          Causal.crochetL (\x0 x1 -> Just (x0-x1, x0)) zero)


{-
{- | needs a good handling of boundaries, yet -}
{-# INLINE meanStatic #-}
meanStatic ::
   (RealRing.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) q   {- ^ cut-off frequency -}
   -> Proc.T s u q (
        SigA.R s v q yv
     -> SigA.R s v q yv)
meanStatic time =
   FiltR.meanStatic time

meanStaticSeparateTY :: (Additive.C yv, Field.C y, RealRing.C t,
         Module.C y yv, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) t   {- ^ cut-off frequency -}
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
meanStaticSeparateTY time =
   -- FiltR.meanStatic time, means that 't' = 'y'
   do f <- toFrequencyScalar time
      return $ \ x ->
         let tInt  = round ((recip f - 1)/2)
             width = tInt*2+1
         in  SigA.processBody
                ((SigA.asTypeOfAmplitude (recip (fromIntegral width)) x *> ) .
                 Delay.staticNeg tInt .
                 MA.sumsStaticInt width) x


{- | needs a better handling of boundaries, yet -}
{-# INLINE mean #-}
mean :: (Additive.C yv, RealRing.C q,
         Module.C q yv, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) q    {- ^ minimum cut-off frequency -}
   -> Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
                              {- v cut-off frequencies -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
mean minFreq =
   FiltR.mean minFreq


{-# INLINE delay #-}
delay :: (Additive.C yv, Field.C y, RealRing.C t, Dim.C u, Dim.C v) =>
      DN.T u t
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
delay time =
   do t <- toTimeScalar time
      return $ SigA.processBody (Delay.static (round t))


{-# INLINE phaseModulation #-}
phaseModulation ::
   (Additive.C yv, RealRing.C q, Dim.C u, Dim.C v,
    Sample.C q, Sample.C yv) =>
      Interpolation.T q yv
   -> DN.T u q
          {- ^ minDelay, minimal delay, may be negative -}
   -> DN.T u q
          {- ^ maxDelay, maximal delay, it must be @minDelay <= maxDelay@
               and the modulation must always be
               in the range [minDelay,maxDelay]. -}
   -> Proc.T s u q (
        SigA.R s u q q
          {- v delay control, positive numbers meanStatic delay,
               negative numbers meanStatic prefetch -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
phaseModulation ip minDelay maxDelay =
   FiltR.phaseModulation ip minDelay maxDelay

{-# INLINE frequencyModulation #-}
frequencyModulation ::
   (Flat.C flat q, Additive.C yv, RealRing.C q, Dim.C u, Dim.C v) =>
      Interpolation.T q yv
   -> Proc.T s u q (
        RP.T s flat q    {- v frequency factors -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
frequencyModulation ip =
   Proc.pure $
      \ factors ->
          SigA.processBody
             (FiltR.interpolateMultiRelativeZeroPad ip (Flat.toSamples factors))

{- |
Frequency modulation where the input signal can have a sample rate
different from the output.
(The sample rate values can differ, the unit must be the same.
We could lift that restriction,
but then the unit handling becomes more complicated,
and I didn't have a use for it so far.)

The function can be used for resampling.
-}
{-# INLINE frequencyModulationDecoupled #-}
frequencyModulationDecoupled ::
   (Flat.C flat q, Additive.C yv, RealRing.C q, Dim.C u, Dim.C v) =>
      Interpolation.T q yv
   -> Proc.T s u q (
        RP.T s flat q    {- v frequency factors -}
     -> SigP.T u q (SigA.D v q SigS.S) yv
     -> SigA.R s v q yv)
frequencyModulationDecoupled ip =
   fmap
      (\toFreq factors y ->
         flip SigA.processBody (RP.fromSignal (SigP.signal y)) $
            FiltR.interpolateMultiRelativeZeroPad ip
               (SigA.scalarSamples toFreq
                  (SigA.fromBody (SigA.actualSampleRate y) (Flat.toSamples factors))))
      (Proc.withParam Proc.toFrequencyScalar)


{- | symmetric phaser -}
{-# INLINE phaser #-}
phaser ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Sample.C q, Sample.C yv) =>
      Interpolation.T q yv
   -> DN.T u q  {- ^ maxDelay, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                {- v delay control -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
phaser = FiltR.phaser

{-# INLINE phaserStereo #-}
phaserStereo ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Sample.C q, Sample.C yv) =>
      Interpolation.T q yv
   -> DN.T u q   {- ^ maxDelay, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                 {- v delay control -}
     -> SigA.R s v q yv
     -> SigA.R s v q (Stereo.T yv))
phaserStereo = FiltR.phaserStereo
-}



{-
The handling of amplitudes is not efficient and the results may surprise.
Due to rounding errors the output amplitude may differ from input amplitude.
This problem can only be overcome by a specialised low-level routine.

allpassPhaser :: (Trans.C q, Module.C q yv, Dim.C u) =>
      NonNeg.Int  {- ^ order, number of filters in the cascade -}
   -> q           {- ^ mixing ratio @x@ means:
                       amplify input by @x@ and
                       amplify delayed signal by @1-x@.
                       Maximum effect is achieved for @x=0.5@. -}
   -> FrequencyFilter s u q (Allpass.Parameter q) amp yv yv
allpassPhaser order r =
-- incomplete
   fmap
      (fmap $ \ap ->
         mix CausalD.<<<
         CausalD.fanout
            (amplify r)
            (amplify (1-r) CausalD.<<< ap))
      (Filt.allpassCascade 20 Filt.allpassFlangerPhase)
-}


{-
{- | Infinitely many equi-delayed exponentially decaying echos. -}
{-# INLINE comb #-}
comb :: (RealRing.C t, Module.C y yv, Dim.C u, Dim.C v, Sample.C yv) =>
   DN.T u t -> y -> Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
comb = FiltR.comb


{- | Infinitely many equi-delayed echos processed by an arbitrary time-preserving signal processor. -}
{-# INLINE combProc #-}
combProc ::
   (RealRing.C t, Absolute.C y, Field.C y, Module.C y yv, Sample.C yv,
    Dim.C u, Dim.C v) =>
   DN.T u t ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv) ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
combProc time proc =
   do f <- proc
      t <- fmap round $ toTimeScalar time
      let chunkSize = SigSt.chunkSize t
      return $ \x ->
         SigA.processBody
            (Sig.fromStorableSignal .
             Comb.runProc t
                (Sig.toStorableSignal chunkSize .
                 SigA.vectorSamples (SigA.toAmplitudeScalar x) .
                 f .
                 SigA.fromBody (SigA.actualAmplitude x) .
                 Sig.fromStorableSignal) .
             Sig.toStorableSignal chunkSize) x
-}


{-# INLINE integrate #-}
integrate :: (Additive.C yv, Field.C q, Dim.C u, Dim.C v) =>
   Proc.T s u q
      (CausalD.T s (Sample.Dimensional v q yv) (Sample.Dimensional (Dim.Mul u v) q yv))
integrate =
   flip fmap Proc.getSampleRate $ \rate ->
      CausalD.consFlip $ \ (Amp.Numeric amp) ->
         (Amp.Numeric $
          DN.unrecip rate &*& amp,
          Integrate.causal)
