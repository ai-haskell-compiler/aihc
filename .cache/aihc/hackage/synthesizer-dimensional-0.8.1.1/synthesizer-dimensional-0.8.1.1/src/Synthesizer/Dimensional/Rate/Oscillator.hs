{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


This module contains various oscillators that respect physical dimensions.
By using the type variable @amp@ we show,
that the oscillators are homogeneous functions.
But since there are even no restrictions on the sample type,
we even show that values from the waveform
go untouched to the output signal.
-}
module Synthesizer.Dimensional.Rate.Oscillator (
   {- * Oscillators with constant waveforms -}
   static,
--   staticAntiAlias,
   freqMod,
--   freqModAntiAlias,
   phaseMod,
   phaseFreqMod,
   shapeMod,
   shapeFreqMod,
   staticSample,
   freqModSample,
--   shapeFreqModSample,
   shapeFreqModFromSampledTone,
   shapePhaseFreqModFromSampledTone,
   ) where

import qualified Synthesizer.Dimensional.Causal.Oscillator as OsciC
import qualified Synthesizer.State.Oscillator as Osci
import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import qualified Synthesizer.Dimensional.Causal.Oscillator.Core as OsciCore
import qualified Synthesizer.Dimensional.Map as MapD

import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
-- import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

-- import qualified Synthesizer.Dimensional.Wave.Smoothed as WaveSmooth
import qualified Synthesizer.Dimensional.Wave.Controlled as WaveCtrl
import qualified Synthesizer.Dimensional.Wave as WaveD
import qualified Synthesizer.Basic.Phase        as Phase

import qualified Synthesizer.Dimensional.Cyclic.Signal as SigC

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (toFrequencyScalar, )

import qualified Synthesizer.Interpolation as Interpolation

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
-- import Number.DimensionTerm ((&*&))

import qualified Algebra.RealField          as RealField

-- import NumericPrelude.Numeric
import NumericPrelude.Base as P



type Signal s amp y =
   SigA.T (Rate.Phantom s) amp (Sig.T y)


{- * Oscillators with constant waveforms -}

{- | oscillator with a functional waveform with constant frequency -}
{-# INLINE static #-}
static ::
   (RealField.C t, Dim.C u) =>
      WaveD.T t (Sample.T amp y)       {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (Signal s amp y)
static wave phase freq =
   fmap (MapD.apply wave) $
   OsciCore.static phase freq


{-
{- | oscillator with a functional waveform with constant frequency -}
{-# INLINE staticAntiAlias #-}
staticAntiAlias ::
   (RealField.C t, Dim.C u,
    Smooth amp t wave sig) =>
      WaveD.T t (Sample.T amp y)
                   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (Signal s amp y)
staticAntiAlias wave phase =
   staticAux (\freq -> withWave wave $ \w -> Osci.staticAntiAlias w phase freq)
-}

{- | oscillator with a functional waveform with modulated frequency -}
{-# INLINE freqMod #-}
freqMod ::
   (RealField.C t, Dim.C u) =>
      WaveD.T t (Sample.T amp y)       {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> Proc.T s u t (
        SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> Signal s amp y)
freqMod wave phase =
   fmap CausalD.apply $
   OsciC.freqMod wave phase

{-
{- | oscillator with a functional waveform with modulated frequency -}
{-# INLINE freqModAntiAlias #-}
freqModAntiAlias ::
   (RealField.C t, Dim.C u,
    Smooth amp t wave sig) =>
      WaveD.T t (Sample.T amp y)
                   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> Proc.T s u t (
        SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> Signal s amp y)
freqModAntiAlias wave phase =
   freqModAux (\t -> withWave wave $ \w -> Osci.freqModAntiAlias w phase t)
-}

{- | oscillator with modulated phase -}
{-# INLINE phaseMod #-}
phaseMod ::
   (Flat.C t flat, RealField.C t, Dim.C u) =>
      WaveD.T t (Sample.T amp y)       {- ^ waveform -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (
        Signal s flat t
                   {- v phase modulation, phases must have no unit -}
     -> Signal s amp y)
phaseMod wave freq =
   fmap CausalD.applyFlat $
   OsciC.phaseMod wave freq

{- | oscillator with modulated shape -}
{-# INLINE shapeMod #-}
shapeMod ::
   (RealField.C t, Dim.C u) =>
      WaveCtrl.T (Sample.T cAmp c) t (Sample.T amp y)
                   {- ^ waveform -}
   -> Phase.T t    {- ^ phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (
        Signal s cAmp c {- v shape control -}
     -> Signal s amp y)
shapeMod wave phase freq =
   fmap CausalD.apply $
   OsciC.shapeMod wave phase freq

{- | oscillator with a functional waveform with modulated phase and frequency -}
{-# INLINE phaseFreqMod #-}
phaseFreqMod ::
   (Flat.C t flat, RealField.C t, Dim.C u) =>
      WaveD.T t (Sample.T amp y)       {- ^ waveform -}
   -> Proc.T s u t (
        Signal s flat t
                     {- v phase control -}
     -> SigA.R s (Dim.Recip u) t t
                     {- v frequency control -}
     -> Signal s amp y)
phaseFreqMod wave =
   flip fmap (OsciC.phaseFreqMod wave) $ \osci phases freqs ->
      CausalD.applyFlatFst osci phases
      `CausalD.apply`
      freqs

{- | oscillator with both shape and frequency modulation -}
{-# INLINE shapeFreqMod #-}
shapeFreqMod :: (RealField.C t, Dim.C u) =>
      WaveCtrl.T (Sample.T cAmp c) t (Sample.T amp y)
                   {- ^ waveform -}
   -> Phase.T t    {- ^ phase -}
   -> Proc.T s u t (
        Signal s cAmp c
                     {- v shape control -}
     -> SigA.R s (Dim.Recip u) t t
                     {- v frequency control -}
     -> Signal s amp y)
shapeFreqMod wave phase =
   flip fmap (OsciC.shapeFreqMod wave phase) $ \osci shapes freqs ->
      CausalD.applyFst osci shapes
      `CausalD.apply`
      freqs


{- |
oscillator with a sampled waveform with constant frequency
This is essentially an interpolation with cyclic padding.
You can also achieve this with a waveform constructed by 'Wave.sample'.
-}
{-# INLINE staticSample #-}
staticSample :: (RealField.C t, Dim.C u) =>
      Interpolation.T t y
   -> SigA.T rate amp (SigC.T (Sig.T y))   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t (Signal s amp y)
staticSample ip wave phase freq =
   flip fmap (toFrequencyScalar freq) $
      SigA.Cons Rate.Phantom (SigA.amplitude wave) .
      Osci.staticSample ip (SigC.toPeriod $ SigA.body wave) phase

{- |
oscillator with a sampled waveform with modulated frequency
Should behave homogenously for different types of interpolation.
-}
{-# INLINE freqModSample #-}
freqModSample :: (RealField.C t, Dim.C u) =>
      Interpolation.T t y
   -> SigA.T rate amp (SigC.T (Sig.T y))   {- ^ waveform -}
   -> Phase.T t    {- ^ start phase -}
   -> Proc.T s u t (
        SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> Signal s amp y)
freqModSample ip wave phase =
   flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
      SigA.Cons Rate.Phantom (SigA.amplitude wave) .
      Osci.freqModSample ip (SigC.toPeriod $ SigA.body wave) phase .
      SigA.scalarSamples toFreq


{-
{-# INLINE shapeFreqModSample #-}
shapeFreqModSample :: (RealField.C c, RealField.C t) =>
      Interpolation.T c (Wave.T t y)
   -> sig (Wave.T t y)
   -> c -> Phase.T t
   -> Proc.T s u t (
        Signal s flat c
                   {- v shape control -}
     -> SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> SigS.R s y)
shapeFreqModSample ip waves shape0 phase =
    uncurry Wave.apply ^<<
       (InterpolationC.relativeConstantPad ip shape0 waves ***
        freqsToPhases phase)
-}

{-# INLINE shapeFreqModFromSampledTone #-}
shapeFreqModFromSampledTone ::
    (RealField.C t, Dim.C u, Flat.C t flat) =>
      Interpolation.T t yv
   -> Interpolation.T t yv
   -> DN.T (Dim.Recip u) t
                   {- ^ source frequency -}
   -> SigA.T (Rate.Dimensional u t) amp (Sig.T yv)
   -> t -> Phase.T t
   -> Proc.T s u t (
        Signal s flat t
                   {- v shape control -}
     -> SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> Signal s amp yv)
shapeFreqModFromSampledTone
      ipLeap ipStep srcFreq sampledTone shape0 phase =
   flip fmap
      (OsciC.shapeFreqModFromSampledTone
         ipLeap ipStep srcFreq sampledTone shape0 phase)
      (\osci ->
         \shapes freqs ->
            osci
            `CausalD.applyFlatFst`
            shapes
            `CausalD.apply`
            freqs)


{-# INLINE shapePhaseFreqModFromSampledTone #-}
shapePhaseFreqModFromSampledTone ::
    (RealField.C t, Dim.C u, Flat.C t flatS, Flat.C t flatP) =>
      Interpolation.T t yv
   -> Interpolation.T t yv
   -> DN.T (Dim.Recip u) t
                   {- ^ source frequency -}
   -> SigA.T (Rate.Dimensional u t) amp (Sig.T yv)
   -> t -> Phase.T t
   -> Proc.T s u t (
        Signal s flatS t
                   {- v shape control -}
     -> Signal s flatP t
                   {- v phase control -}
     -> SigA.R s (Dim.Recip u) t t
                   {- v frequency control -}
     -> Signal s amp yv)
shapePhaseFreqModFromSampledTone
      ipLeap ipStep srcFreq sampledTone shape0 phase =
   flip fmap
      (OsciC.shapePhaseFreqModFromSampledTone
         ipLeap ipStep srcFreq sampledTone shape0 phase)
      (\osci ->
         \shapes phaseDistort freqs ->
            (osci CausalD.<<^ MapD.packTriple)
            `CausalD.applyFlatFst`
            shapes
            `CausalD.applyFlatFst`
            phaseDistort
            `CausalD.apply`
            freqs)
