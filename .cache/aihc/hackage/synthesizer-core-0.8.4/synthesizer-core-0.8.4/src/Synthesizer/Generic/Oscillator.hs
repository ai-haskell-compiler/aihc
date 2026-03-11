{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Tone generators

Frequencies are always specified in ratios of the sample rate,
e.g. the frequency 0.01 for the sample rate 44100 Hz
means a physical frequency of 441 Hz.
-}
module Synthesizer.Generic.Oscillator where

import qualified Synthesizer.State.Oscillator as OsciS
import qualified Synthesizer.Causal.Oscillator as OsciC
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Basic.Phase      as Phase

import qualified Synthesizer.Causal.Interpolation as Interpolation

import qualified Synthesizer.Generic.Signal  as SigG

import Control.Arrow ((>>>), )

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField

import NumericPrelude.Numeric
import NumericPrelude.Base



{- * Oscillators with arbitrary but constant waveforms -}

{- | oscillator with constant frequency -}
static :: (RealField.C a, SigG.Write sig b) =>
   SigG.LazySize ->
   Wave.T a b -> (Phase.T a -> a -> sig b)
static size wave phase freq =
   SigG.fromState size (OsciS.static wave phase freq)

{- | oscillator with modulated frequency -}
freqMod :: (RealField.C a, SigG.Transform sig a, SigG.Transform sig b) =>
   Wave.T a b -> Phase.T a -> sig a -> sig b
freqMod wave phase =
   Causal.apply (OsciC.freqMod wave phase)

{- | oscillator with modulated phase -}
phaseMod :: (RealField.C a, SigG.Transform sig a, SigG.Transform sig b) =>
   Wave.T a b -> a -> sig a -> sig b
phaseMod wave =
   shapeMod (Wave.phaseOffset wave) zero

{- | oscillator with modulated shape -}
shapeMod :: (RealField.C a, SigG.Transform sig c, SigG.Transform sig b) =>
   (c -> Wave.T a b) -> Phase.T a -> a -> sig c -> sig b
shapeMod wave phase freq =
   Causal.apply (OsciC.shapeMod wave phase freq)

{- | oscillator with both phase and frequency modulation -}
phaseFreqMod :: (RealField.C a, SigG.Transform sig a, SigG.Transform sig b) =>
   Wave.T a b -> sig a -> sig a -> sig b
phaseFreqMod wave =
   shapeFreqMod (Wave.phaseOffset wave) zero

{- | oscillator with both shape and frequency modulation -}
shapeFreqMod ::
   (RealField.C a,
    SigG.Read sig c, SigG.Transform sig a, SigG.Transform sig b) =>
   (c -> Wave.T a b) -> Phase.T a -> sig c -> sig a -> sig b
shapeFreqMod wave phase parameters =
   Causal.apply
      (Causal.feedGenericFst parameters >>>
       OsciC.shapeFreqMod wave phase)


{- | oscillator with a sampled waveform with constant frequency
This is essentially an interpolation with cyclic padding.
-}
staticSample :: (RealField.C a, SigG.Read wave b, SigG.Write sig b) =>
   SigG.LazySize ->
   Interpolation.T a b -> wave b -> Phase.T a -> a -> sig b
staticSample size ip wave phase freq =
   let len = fromIntegral $ SigG.length wave
   in  SigG.fromState size $
       Interpolation.relativeCyclicPad
          ip (len * Phase.toRepresentative phase)
          (SigG.toState wave)
       `Causal.applyConst`
       (freq * len)

{- | oscillator with a sampled waveform with modulated frequency
Should behave homogenously for different types of interpolation.
-}
freqModSample ::
   (RealField.C a,
    SigG.Read wave b, SigG.Transform sig a, SigG.Transform sig b) =>
   Interpolation.T a b -> wave b -> Phase.T a -> sig a -> sig b
freqModSample ip wave phase freqs =
   let len = fromIntegral $ SigG.length wave
   in  Interpolation.relativeCyclicPad
          ip (len * Phase.toRepresentative phase)
          (SigG.toState wave)
       `Causal.apply`
       SigG.map (* len) freqs


{-
Shape+phase modulating oscillators can be found in Causal.Oscillator.
-}

{- * Oscillators with specific waveforms -}

{- | sine oscillator with static frequency -}
staticSine :: (Trans.C a, RealField.C a, SigG.Write sig a) =>
   SigG.LazySize ->
   Phase.T a -> a -> sig a
staticSine size =
   static size Wave.sine

{- | sine oscillator with modulated frequency -}
freqModSine :: (Trans.C a, RealField.C a, SigG.Transform sig a) =>
   Phase.T a -> sig a -> sig a
freqModSine phase =
   Causal.applySameType (OsciC.freqMod Wave.sine phase)

{- | sine oscillator with modulated phase, useful for FM synthesis -}
phaseModSine :: (Trans.C a, RealField.C a, SigG.Transform sig a) =>
   a -> sig a -> sig a
phaseModSine freq =
   Causal.applySameType (OsciC.phaseMod Wave.sine freq)

{- | saw tooth oscillator with modulated frequency -}
staticSaw :: (RealField.C a, SigG.Write sig a) =>
   SigG.LazySize ->
   Phase.T a -> a -> sig a
staticSaw size =
   static size Wave.saw

{- | saw tooth oscillator with modulated frequency -}
freqModSaw :: (RealField.C a, SigG.Transform sig a) =>
   Phase.T a -> sig a -> sig a
freqModSaw phase =
   Causal.applySameType (OsciC.freqMod Wave.saw phase)
