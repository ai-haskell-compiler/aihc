{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Tone generators
-}
module Synthesizer.State.Oscillator where

import qualified Synthesizer.Causal.Oscillator  as Osci
import qualified Synthesizer.Causal.Oscillator.Core as OsciCore
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Basic.WaveSmoothed as WaveSmooth
import qualified Synthesizer.Basic.Wave         as Wave
import qualified Synthesizer.Basic.Phase        as Phase

import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Interpolation as Interpolation

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.RealRing              as RealRing

import NumericPrelude.Numeric (Float, Double, )



{- * Oscillators with arbitrary but constant waveforms -}

{-# INLINE static #-}
{- |
Oscillator with constant frequency.
It causes aliasing effects for sharp waveforms and high frequencies.
-}
static :: (RealRing.C a) => Wave.T a b -> (Phase.T a -> a -> Sig.T b)
static wave phase freq =
    Sig.map (Wave.apply wave) (OsciCore.static phase freq)

{-# INLINE staticAntiAlias #-}
{- |
Oscillator with constant frequency
that suppresses aliasing effects using waveforms with controllable smoothness.
-}
staticAntiAlias :: (RealRing.C a) =>
    WaveSmooth.T a b -> (Phase.T a -> a -> Sig.T b)
staticAntiAlias wave phase freq =
    Sig.map (WaveSmooth.apply wave freq) (OsciCore.static phase freq)

{-# INLINE phaseMod #-}
{- | oscillator with modulated phase -}
phaseMod :: (RealRing.C a) => Wave.T a b -> a -> Sig.T a -> Sig.T b
phaseMod wave freq =
    Causal.apply (Osci.phaseMod wave freq)

{-# INLINE shapeMod #-}
{- | oscillator with modulated shape -}
shapeMod :: (RealRing.C a) =>
    (c -> Wave.T a b) -> Phase.T a -> a -> Sig.T c -> Sig.T b
shapeMod wave phase freq =
    Causal.apply (Osci.shapeMod wave phase freq)

{-# INLINE freqMod #-}
{- | oscillator with modulated frequency -}
freqMod :: (RealRing.C a) => Wave.T a b -> Phase.T a -> Sig.T a -> Sig.T b
freqMod wave phase =
    Causal.apply (Osci.freqMod wave phase)

{-# INLINE freqModAntiAlias #-}
{- | oscillator with modulated frequency -}
freqModAntiAlias :: (RealRing.C a) =>
    WaveSmooth.T a b -> Phase.T a -> Sig.T a -> Sig.T b
freqModAntiAlias wave phase =
    Causal.apply (Osci.freqModAntiAlias wave phase)

{-# INLINE phaseFreqMod #-}
{- | oscillator with both phase and frequency modulation -}
phaseFreqMod :: (RealRing.C a) =>
    Wave.T a b -> Sig.T a -> Sig.T a -> Sig.T b
phaseFreqMod wave =
    Causal.apply2 (Osci.phaseFreqMod wave)

{-# INLINE shapeFreqMod #-}
{- | oscillator with both shape and frequency modulation -}
shapeFreqMod :: (RealRing.C a) =>
    (c -> Wave.T a b) -> Phase.T a -> Sig.T c -> Sig.T a -> Sig.T b
shapeFreqMod wave phase =
    Causal.apply2 (Osci.shapeFreqMod wave phase)


{- | oscillator with a sampled waveform with constant frequency
     This essentially an interpolation with cyclic padding. -}
{-# INLINE staticSample #-}
staticSample :: RealRing.C a =>
    Interpolation.T a b -> Sig.T b -> Phase.T a -> a -> Sig.T b
staticSample ip wave phase freq =
    Causal.apply (Osci.freqModSample ip wave phase) (Sig.repeat freq)

{- | oscillator with a sampled waveform with modulated frequency
     Should behave homogenously for different types of interpolation. -}
{-# INLINE freqModSample #-}
freqModSample :: RealRing.C a =>
    Interpolation.T a b -> Sig.T b -> Phase.T a -> Sig.T a -> Sig.T b
freqModSample ip wave phase =
    Causal.apply (Osci.freqModSample ip wave phase)

{-# INLINE shapeFreqModSample #-}
shapeFreqModSample :: (RealRing.C c, RealRing.C a) =>
    Interpolation.T c (Wave.T a b) -> Sig.T (Wave.T a b) ->
    c -> Phase.T a ->
    Sig.T c -> Sig.T a -> Sig.T b
shapeFreqModSample ip waves shape0 phase =
    Causal.apply2 (Osci.shapeFreqModSample ip waves shape0 phase)

{-# INLINE shapeFreqModFromSampledTone #-}
shapeFreqModFromSampledTone ::
    (RealField.C a, SigG.Transform sig b) =>
    Interpolation.T a b ->
    Interpolation.T a b ->
    a -> sig b ->
    a -> Phase.T a ->
    Sig.T a -> Sig.T a -> Sig.T b
shapeFreqModFromSampledTone
      ipLeap ipStep period sampledTone shape0 phase =
    Causal.apply2
       (Osci.shapeFreqModFromSampledTone
          ipLeap ipStep period sampledTone shape0 phase)

{-# INLINE shapePhaseFreqModFromSampledTone #-}
shapePhaseFreqModFromSampledTone ::
    (RealField.C a, SigG.Transform sig b) =>
    Interpolation.T a b ->
    Interpolation.T a b ->
    a -> sig b ->
    a -> Phase.T a ->
    Sig.T a -> Sig.T a -> Sig.T a -> Sig.T b
shapePhaseFreqModFromSampledTone
      ipLeap ipStep period sampledTone shape0 phase =
    Causal.apply3
       (Osci.shapePhaseFreqModFromSampledTone
          ipLeap ipStep period sampledTone shape0 phase)



{- * Oscillators with specific waveforms -}

{-# INLINE staticSine #-}
{-# SPECIALISE INLINE staticSine :: Phase.T Float -> Float -> Sig.T Float #-}
{-# SPECIALISE INLINE staticSine :: Phase.T Double -> Double -> Sig.T Double #-}
{- | sine oscillator with static frequency -}
staticSine :: (Trans.C a, RealRing.C a) => Phase.T a -> a -> Sig.T a
staticSine = static Wave.sine

{-# INLINE freqModSine #-}
{-# SPECIALISE INLINE freqModSine :: Phase.T Float -> Sig.T Float -> Sig.T Float #-}
{-# SPECIALISE INLINE freqModSine :: Phase.T Double -> Sig.T Double -> Sig.T Double #-}
{- | sine oscillator with modulated frequency -}
freqModSine :: (Trans.C a, RealRing.C a) => Phase.T a -> Sig.T a -> Sig.T a
freqModSine = freqMod Wave.sine

{-# INLINE phaseModSine #-}
{-# SPECIALISE INLINE phaseModSine :: Float -> Sig.T Float -> Sig.T Float #-}
{-# SPECIALISE INLINE phaseModSine :: Double -> Sig.T Double -> Sig.T Double #-}
{- | sine oscillator with modulated phase, useful for FM synthesis -}
phaseModSine :: (Trans.C a, RealRing.C a) => a -> Sig.T a -> Sig.T a
phaseModSine = phaseMod Wave.sine

{-# INLINE staticSaw #-}
{-# SPECIALISE INLINE staticSaw :: Phase.T Float -> Float -> Sig.T Float #-}
{-# SPECIALISE INLINE staticSaw :: Phase.T Double -> Double -> Sig.T Double #-}
{- | saw tooth oscillator with modulated frequency -}
staticSaw :: RealRing.C a => Phase.T a -> a -> Sig.T a
staticSaw = static Wave.saw

{-# INLINE freqModSaw #-}
{-# SPECIALISE INLINE freqModSaw :: Phase.T Float -> Sig.T Float -> Sig.T Float #-}
{-# SPECIALISE INLINE freqModSaw :: Phase.T Double -> Sig.T Double -> Sig.T Double #-}
{- | saw tooth oscillator with modulated frequency -}
freqModSaw :: RealRing.C a => Phase.T a -> Sig.T a -> Sig.T a
freqModSaw = freqMod Wave.saw
