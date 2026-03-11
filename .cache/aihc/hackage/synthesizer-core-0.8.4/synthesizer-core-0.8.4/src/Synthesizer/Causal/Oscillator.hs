{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Tone generators
-}
module Synthesizer.Causal.Oscillator where

import qualified Synthesizer.Causal.Oscillator.Core as Osci
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Causal.Interpolation as InterpolationC
import qualified Synthesizer.Causal.ToneModulation as ToneMod

import qualified Synthesizer.Basic.WaveSmoothed as WaveSmooth
import qualified Synthesizer.Basic.Wave         as Wave
import qualified Synthesizer.Basic.Phase        as Phase

import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.RealRing              as RealRing

import Control.Arrow ((^<<), (<<^), (<<<), (***), )

import NumericPrelude.Numeric
import NumericPrelude.Base



{- * Oscillators with arbitrary but constant waveforms -}

{-
{-# INLINE static #-}
{- | oscillator with constant frequency -}
static :: (RealRing.C a) =>
   Wave.T a b -> (Phase.T a -> a -> Sig.T b)
static wave phase freq =
   Sig.map (Wave.apply wave) (Osci.static phase freq)
-}


{-# INLINE phaseMod #-}
{- | oscillator with modulated phase -}
phaseMod :: (RealRing.C a) =>
   Wave.T a b -> a -> Causal.T a b
phaseMod wave freq =
   Wave.apply wave ^<< Osci.phaseMod freq

{-# INLINE shapeMod #-}
{- | oscillator with modulated shape -}
shapeMod :: (RealRing.C a) =>
   (c -> Wave.T a b) -> Phase.T a -> a -> Causal.T c b
shapeMod wave phase freq =
   uncurry (Wave.apply . wave) ^<<
   Osci.shapeMod phase freq


{-# INLINE freqMod #-}
{- | oscillator with modulated frequency -}
freqMod :: (RealRing.C a) =>
   Wave.T a b -> Phase.T a -> Causal.T a b
freqMod wave phase =
   Wave.apply wave ^<< Osci.freqMod phase

{-# INLINE freqModAntiAlias #-}
{- | oscillator with modulated frequency -}
freqModAntiAlias :: (RealRing.C a) =>
   WaveSmooth.T a b -> Phase.T a -> Causal.T a b
freqModAntiAlias wave phase =
   uncurry (WaveSmooth.apply wave) ^<<
   Osci.freqModAntiAlias phase

{-# INLINE phaseFreqMod #-}
{- | oscillator with both phase and frequency modulation -}
phaseFreqMod :: (RealRing.C a) =>
   Wave.T a b -> Causal.T (a,a) b
phaseFreqMod wave =
   Wave.apply wave ^<< Osci.phaseFreqMod

{-# INLINE shapeFreqMod #-}
{- | oscillator with both shape and frequency modulation -}
shapeFreqMod :: (RealRing.C a) =>
   (c -> Wave.T a b) -> Phase.T a -> Causal.T (c,a) b
shapeFreqMod wave phase =
   uncurry (Wave.apply . wave) ^<<
   Osci.shapeFreqMod phase


{-
{- | oscillator with a sampled waveform with constant frequency
    This essentially an interpolation with cyclic padding. -}
{-# INLINE staticSample #-}
staticSample :: RealRing.C a =>
   Interpolation.T a b -> Sig.T b -> Phase.T a -> a -> Sig.T b
staticSample ip wave phase freq =
   Causal.apply (freqModSample ip wave phase) (Sig.repeat freq)
-}

{- | oscillator with a sampled waveform with modulated frequency
    Should behave homogenously for different types of interpolation. -}
{-# INLINE freqModSample #-}
freqModSample :: RealRing.C a =>
   Interpolation.T a b -> Sig.T b -> Phase.T a -> Causal.T a b
freqModSample ip wave phase =
   let len = Sig.length wave
       pr  = fromIntegral len * Phase.toRepresentative phase
   in  InterpolationC.relativeCyclicPad ip pr wave
         <<< Causal.map (fromIntegral len *)


{-# INLINE shapeFreqModSample #-}
shapeFreqModSample :: (RealRing.C c, RealRing.C b) =>
   Interpolation.T c (Wave.T b a) -> Sig.T (Wave.T b a) ->
   c -> Phase.T b ->
   Causal.T (c, b) a
shapeFreqModSample ip waves shape0 phase =
   uncurry Wave.apply ^<<
      (InterpolationC.relativeConstantPad ip shape0 waves ***
       Osci.freqMod phase)

{-# INLINE shapeFreqModFromSampledTone #-}
shapeFreqModFromSampledTone ::
   (RealField.C t, SigG.Transform sig y) =>
   Interpolation.T t y ->
   Interpolation.T t y ->
   t -> sig y ->
   t -> Phase.T t ->
   Causal.T (t,t) y
shapeFreqModFromSampledTone
      ipLeap ipStep period sampledTone shape0 phase =
   uncurry (ToneMod.interpolateCell ipLeap ipStep) ^<<
   ToneMod.oscillatorCells
      (Interpolation.margin ipLeap) (Interpolation.margin ipStep)
      (round period) period sampledTone
      (shape0, phase)

{-# INLINE shapePhaseFreqModFromSampledTone #-}
shapePhaseFreqModFromSampledTone ::
   (RealField.C t, SigG.Transform sig y) =>
   Interpolation.T t y ->
   Interpolation.T t y ->
   t -> sig y ->
   t -> Phase.T t ->
   Causal.T (t,t,t) y
shapePhaseFreqModFromSampledTone
      ipLeap ipStep period sampledTone shape0 phase =
   let periodInt = round period
       marginLeap = Interpolation.margin ipLeap
       marginStep = Interpolation.margin ipStep
   in  (\(dp, ((s,p), suffix)) ->
          uncurry (ToneMod.interpolateCell ipLeap ipStep) $
          ToneMod.seekCell periodInt period $
          ((s, Phase.increment dp p), suffix))
       ^<<
       Causal.second
          (ToneMod.oscillatorSuffixes
             marginLeap marginStep
             periodInt period sampledTone
             (shape0, phase))
       <<^
       (\(s,p,f) -> (p,(s,f)))


{- * Oscillators with specific waveforms -}

{-
{-# INLINE staticSine #-}
{- | sine oscillator with static frequency -}
staticSine :: (Trans.C a, RealField.C a) => Phase.T a -> a -> Sig.T a
staticSine = static Wave.sine
-}

{-# INLINE freqModSine #-}
{- | sine oscillator with modulated frequency -}
freqModSine :: (Trans.C a, RealRing.C a) => Phase.T a -> Causal.T a a
freqModSine = freqMod Wave.sine

{-# INLINE phaseModSine #-}
{- | sine oscillator with modulated phase, useful for FM synthesis -}
phaseModSine :: (Trans.C a, RealRing.C a) => a -> Causal.T a a
phaseModSine = phaseMod Wave.sine

{-
{-# INLINE staticSaw #-}
{- | saw tooth oscillator with modulated frequency -}
staticSaw :: RealRing.C a => Phase.T a -> a -> Sig.T a
staticSaw = static Wave.saw
-}

{-# INLINE freqModSaw #-}
{- | saw tooth oscillator with modulated frequency -}
freqModSaw :: RealRing.C a => Phase.T a -> Causal.T a a
freqModSaw = freqMod Wave.saw
