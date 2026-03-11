{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2009-2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

-}
module Synthesizer.Dimensional.Causal.Oscillator (
{-
   static,
   staticAntiAlias,
-}
   freqMod,
{-
   freqModAntiAlias,
-}
   phaseMod,
   phaseFreqMod,
   shapeMod,
   shapeFreqMod,
{-
   staticSample,
   freqModSample,
-}
--   shapeFreqModSample,
   shapeFreqModFromSampledTone,
   shapePhaseFreqModFromSampledTone,
   ) where

import qualified Synthesizer.Dimensional.Causal.Oscillator.Core as OsciCore
import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import Control.Arrow ((<<^), (<<<), second, )

import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Causal.Oscillator as Osci
import Synthesizer.Causal.Filter.NonRecursive (amplify, )

import qualified Synthesizer.Generic.Signal as SigG

-- import qualified Synthesizer.Dimensional.Wave.Smoothed as WaveSmooth
import qualified Synthesizer.Dimensional.Wave.Controlled as WaveCtrl
import qualified Synthesizer.Dimensional.Wave as WaveD
import qualified Synthesizer.Basic.Phase        as Phase

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (toFrequencyScalar, )

import qualified Synthesizer.Interpolation as Interpolation

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.RealField          as RealField

import NumericPrelude.Numeric
import NumericPrelude.Base as P


type Frequency u t = Amp.Numeric (DN.T (Dim.Recip u) t)
type SampleFrequency u t = Sample.T (Frequency u t) t


{-
{- | oscillator with a functional waveform with constant frequency -}
{-# INLINE static #-}
static :: (RealField.C t, Dim.C u) =>
      WaveD.T amp t y   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (SigS.R s y)
static wave phase =
   staticAuxHom (SigS.fromSamples . Osci.static wave phase)

{- | oscillator with a functional waveform with constant frequency -}
{-# INLINE staticAntiAlias #-}
staticAntiAlias :: (RealField.C t, Dim.C u) =>
      WaveSmooth.T amp t y
                   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (SigS.R s y)
staticAntiAlias wave phase =
   staticAuxHom (SigS.fromSamples . Osci.staticAntiAlias wave phase)
-}

{- | oscillator with a functional waveform with modulated frequency -}
{-# INLINE freqMod #-}
freqMod :: (RealField.C t, Dim.C u) =>
      WaveD.T t y   {- ^ waveform -}
   -> Phase.T t        {- ^ start phase -}
   -> Proc.T s u t
         (CausalD.T s (SampleFrequency u t) y)
freqMod wave phase =
   fmap (wave CausalD.^<<) $ OsciCore.freqMod phase

{-
{- | oscillator with a functional waveform with modulated frequency -}
{-# INLINE freqModAntiAlias #-}
freqModAntiAlias :: (RealField.C t, Dim.C u) =>
      WaveSmooth.T amp t y
                   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> Proc.T s u t
         (CausalD.T s (Frequency u t) amp t y)
freqModAntiAlias wave phase =
   freqModAuxHom wave $ \scaleFreq freqAmp w ->
      Osci.freqModAntiAlias w phase <<< scaleFreq freqAmp
-}

{- | oscillator with modulated phase -}
{-# INLINE phaseMod #-}
phaseMod :: (RealField.C t, Dim.C u) =>
      WaveD.T t y       {- ^ waveform -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t
         (CausalD.T s (Sample.Flat t) y)
phaseMod wave freq =
   fmap (wave CausalD.^<<) $
   OsciCore.phaseMod freq


{- | oscillator with modulated shape -}
{-# INLINE shapeMod #-}
shapeMod :: (RealField.C t, Dim.C u) =>
      WaveCtrl.T c t y
                   {- ^ waveform -}
   -> Phase.T t    {- ^ phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t
         (CausalD.T s c y)
shapeMod wave phase freq =
   fmap (wave CausalD.^<<) $
   fmap CausalD.feedSnd $
   OsciCore.static phase freq


{- | oscillator with a functional waveform with modulated phase and frequency -}
{-# INLINE phaseFreqMod #-}
phaseFreqMod :: (RealField.C t, Dim.C u) =>
      WaveD.T t y   {- ^ waveform -}
   -> Proc.T s u t
         (CausalD.T s (Sample.Flat t, SampleFrequency u t) y)
phaseFreqMod wave =
   fmap (wave CausalD.^<<) $
   OsciCore.phaseFreqMod


{- | oscillator with both shape and frequency modulation -}
{-# INLINE shapeFreqMod #-}
shapeFreqMod :: (RealField.C t, Dim.C u) =>
      WaveCtrl.T c t y
                   {- ^ waveform -}
   -> Phase.T t    {- ^ phase -}
   -> Proc.T s u t
         (CausalD.T s (c, SampleFrequency u t) y)
shapeFreqMod wave phase =
   fmap (wave CausalD.^<<) $
   fmap second $
   OsciCore.freqMod phase


{-
We could decouple source time and target time which yields

      DN.T (Dim.Recip u0) t
                   {- ^ source frequency -}
   -> SigP.T u0 (SigA.D v y (SigS.T sig)) y
   -> t -> Phase.T t
   -> Proc.T s u1 t (
        CausalD.T s (DN.T (Dim.Div u0 u1) t, DN.T (Dim.Recip u1) t) Amp.Flat (t,t) y)

but most oftenly we do not need the conversion of the time scale.
If we need it, we can use the frequency modulation function.

We could measure the shape parameter in multiples of the source wave period.
This would yield

      DN.T (Dim.Recip u0) t
                   {- ^ source frequency -}
   -> SigP.T u0 (SigA.D v y (SigS.T sig)) y
   -> t -> Phase.T t
   -> Proc.T s u1 t (
        CausalD.T s (DN.T (Dim.Recip u1) t, DN.T (Dim.Recip u1) t) Amp.Flat (t,t) y)

but this way, adjustment of the shape parameter is coupled to the source period.
-}
{-# INLINE shapeFreqModFromSampledTone #-}
shapeFreqModFromSampledTone ::
    (RealField.C t, SigG.Transform sig yv, Dim.C u) =>
      Interpolation.T t yv
   -> Interpolation.T t yv
   -> DN.T (Dim.Recip u) t
                   {- ^ source frequency -}
   -> SigA.T (Rate.Dimensional u t) amp (sig yv)
   -> t -> Phase.T t
   -> Proc.T s u t
         (CausalD.T s
             (Sample.Flat t, SampleFrequency u t)
             (Sample.T amp yv))
shapeFreqModFromSampledTone
      ipLeap ipStep srcFreq sampledTone shape0 phase =
   let SigA.Cons (Rate.Actual srcRate) amp samples = sampledTone
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
       CausalD.consFlip $ \(Amp.Flat, Amp.Numeric freqAmp) ->
        (amp,
         Osci.shapeFreqModFromSampledTone
            ipLeap ipStep
            (DN.divToScalar srcRate srcFreq)
            samples
            shape0 phase
          <<< second (amplify (toFreq freqAmp)))


{-# INLINE shapePhaseFreqModFromSampledTone #-}
shapePhaseFreqModFromSampledTone ::
    (RealField.C t, SigG.Transform sig yv, Dim.C u) =>
      Interpolation.T t yv
   -> Interpolation.T t yv
   -> DN.T (Dim.Recip u) t
                   {- ^ source frequency -}
   -> SigA.T (Rate.Dimensional u t) amp (sig yv)
   -> t -> Phase.T t
   -> Proc.T s u t
         (CausalD.T s
             (Sample.Flat t, Sample.Flat t, SampleFrequency u t)
             (Sample.T amp yv))
shapePhaseFreqModFromSampledTone
      ipLeap ipStep srcFreq sampledTone shape0 phase =
   let SigA.Cons (Rate.Actual srcRate) amp samples = sampledTone
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
       CausalD.consFlip $ \(Amp.Flat, Amp.Flat, Amp.Numeric freqAmp) ->
        (amp,
         Osci.shapePhaseFreqModFromSampledTone
            ipLeap ipStep
            (DN.divToScalar srcRate srcFreq)
            samples
            shape0 phase
          <<^
          (\(s,p,f) -> (s,p, toFreq freqAmp * f)))
{-
          Causal.packTriple
          ^<<
          second (amplify (toFreq freqAmp))
          <<^
          Causal.unpackTriple
-}
