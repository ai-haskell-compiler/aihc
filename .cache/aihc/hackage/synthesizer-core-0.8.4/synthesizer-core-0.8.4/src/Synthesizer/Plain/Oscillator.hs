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
module Synthesizer.Plain.Oscillator where

import qualified Synthesizer.Plain.ToneModulation as ToneMod
import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Basic.Phase as Phase
import qualified Synthesizer.Plain.Interpolation as Interpolation
import qualified Synthesizer.Plain.Signal as Sig

import Synthesizer.Plain.ToneModulation (freqsToPhases, )

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.RealRing              as RealRing

import Data.Tuple.HT (mapFst, mapSnd, )

import NumericPrelude.Numeric
import NumericPrelude.Base


type Phase a = a


{- * Oscillators with arbitrary but constant waveforms -}

{- | oscillator with constant frequency -}
static :: (RealRing.C a) => Wave.T a b -> (Phase a -> a -> Sig.T b)
static wave phase freq =
    map (Wave.apply wave)
        (iterate (Phase.increment freq) (Phase.fromRepresentative phase))

{- | oscillator with modulated frequency -}
freqMod :: (RealRing.C a) => Wave.T a b -> Phase a -> Sig.T a -> Sig.T b
freqMod wave phase freqs =
    map (Wave.apply wave)
        (freqsToPhases (Phase.fromRepresentative phase) freqs)

{- | oscillator with modulated phase -}
phaseMod :: (RealRing.C a) => Wave.T a b -> a -> Sig.T (Phase a) -> Sig.T b
phaseMod wave freq phases =
    map (Wave.apply wave) $
    zipWith Phase.increment phases (iterate (Phase.increment freq) zero)

{- | oscillator with modulated shape -}
shapeMod ::
    (RealRing.C a) => (c -> Wave.T a b) -> (Phase a) -> a -> Sig.T c -> Sig.T b
shapeMod wave phase freq parameters =
    zipWith (Wave.apply . wave) parameters $
    iterate (Phase.increment freq) (Phase.fromRepresentative phase)

{- | oscillator with both phase and frequency modulation -}
phaseFreqMod ::
    (RealRing.C a) => Wave.T a b -> Sig.T (Phase a) -> Sig.T a -> Sig.T b
phaseFreqMod wave phases freqs =
    map (Wave.apply wave)
        (zipWith Phase.increment phases (freqsToPhases zero freqs))

{- | oscillator with both shape and frequency modulation -}
shapeFreqMod ::
    (RealRing.C a) =>
    (c -> Wave.T a b) -> Phase a -> Sig.T c -> Sig.T a -> Sig.T b
shapeFreqMod wave phase parameters freqs =
    zipWith (Wave.apply . wave) parameters $
    freqsToPhases (Phase.fromRepresentative phase) freqs


{- | oscillator with a sampled waveform with constant frequency
     This is essentially an interpolation with cyclic padding. -}
staticSample ::
    (RealRing.C a) =>
    Interpolation.T a b -> [b] -> Phase a -> a -> Sig.T b
staticSample ip wave phase freq =
    freqModSample ip wave phase (repeat freq)

{- | oscillator with a sampled waveform with modulated frequency
     Should behave homogenously for different types of interpolation. -}
freqModSample ::
    (RealRing.C a) =>
    Interpolation.T a b -> [b] -> Phase a -> Sig.T a -> Sig.T b
freqModSample ip wave phase freqs =
    let len = fromIntegral (length wave)
    in  Interpolation.multiRelativeCyclicPad
           ip (phase*len) (map (len*) freqs) wave

{- |
Shape control is a list of relative changes,
each of which must be non-negative in order to allow lazy processing.
'1' advances by one wave.
Frequency control can be negative.
If you want to use sampled waveforms as well
then use 'Wave.sample' in the list of waveforms.
With sampled waves this function is identical to HunkTranspose in Assampler.

Example: interpolate different versions
of 'Wave.oddCosine' and 'Wave.oddTriangle'.

You could also chop a tone into single waves
and use the waves as input for this function
but you certainly want to use
'Wave.sampledTone' or 'shapeFreqModFromSampledTone' instead,
because in the wave information for 'shapeFreqModSample'
shape and phase are strictly separated.
-}
shapeFreqModSample ::
    (RealRing.C c, RealRing.C b) =>
    Interpolation.T c (Wave.T b a) ->
    [Wave.T b a] -> c -> Phase b -> Sig.T c -> Sig.T b -> Sig.T a
shapeFreqModSample ip waves shape0 phase shapes freqs =
    zipWith Wave.apply
       (Interpolation.multiRelativeConstantPad ip shape0 shapes waves)
       (freqsToPhases (Phase.fromRepresentative phase) freqs)
{-
GNUPlot.plotList [] $ take 500 $ shapeFreqModSample Interpolation.cubic (map Wave.truncOddCosine [0..3]) (0.1::Double) (0::Double) (repeat 0.005) (repeat 0.02)
-}

shapePhaseFreqModSample ::
    (RealRing.C c, RealRing.C b) =>
    Interpolation.T c (Wave.T b a) ->
    [Wave.T b a] -> c -> Sig.T c -> Sig.T (Phase b) -> Sig.T b -> Sig.T a
shapePhaseFreqModSample ip waves shape0 shapes phases freqs =
    zipWith Wave.apply
       (Interpolation.multiRelativeConstantPad ip shape0 shapes waves)
       (zipWith Phase.increment phases (freqsToPhases zero freqs))

{- |
Time stretching and frequency modulation of a pure tone.

We consider a tone as the result of a shape modulated oscillator,
and virtually reconstruct the waveform function
(a function of time and phase) by interpolation and resample it.
This way we can alter frequency and time progress of the tone independently.

This function is identical to using 'shapeFreqMod'
with a wave function constructed by 'Wave.sampledTone'
but it consumes the sampled source tone lazily
and thus allows only relative shape control with non-negative control steps.

The function is similar to 'shapeFreqModSample' but respects
that in a sampled tone, phase and shape control advance synchronously.
Actually we could re-use 'shapeFreqModSample' with modified phase values.
But we would have to cope with negative shape control jumps,
and waves would be padded locally cyclically.
The latter one is not wanted
since we want padding according to the adjacencies in the source tone.
Note that differently from 'shapeFreqModSample'
the shape control difference @1@ does not mean to skip to the next wave,
since this oscillator has no discrete waveforms.
Instead @1@ means that the shape alters as fast as in the prototype signal.

Although the shape difference values must be non-negative
I hesitate to give them the type @Number.NonNegative.T t@
because then you cannot call this function with other types
of non-negative numbers like 'Number.NonNegativeChunky.T'.

The prototype tone signal is reproduced if
@freqs == repeat (1\/period)@ and @shapes == repeat 1@.
-}
shapeFreqModFromSampledTone :: (RealField.C t) =>
    Interpolation.T t y ->
    Interpolation.T t y ->
    t -> Sig.T y -> t -> t -> Sig.T t -> Sig.T t -> Sig.T y
shapeFreqModFromSampledTone
      ipLeap ipStep period sampledTone
      shape0 phase shapes freqs =
   let periodInt = round period
   in  map
          (uncurry (ToneMod.interpolateCell ipLeap ipStep))
          (ToneMod.oscillatorCells
              (Interpolation.margin ipLeap) (Interpolation.margin ipStep)
              periodInt period sampledTone
              (shape0, shapes) (Phase.fromRepresentative phase, freqs))
{-
GNUPlot.plotList [] $ take 1000 $ shapeFreqModFromSampledTone Interpolation.linear Interpolation.linear (1/0.07::Double) (staticSine (0::Double) 0.07) 0 0 (repeat 0.1) (repeat 0.01)
GNUPlot.plotList [] $ take 1000 $ shapeFreqModFromSampledTone Interpolation.linear Interpolation.linear (1/0.07::Double) (staticSine (0::Double) 0.07) 0 0 (repeat 0.1) (iterate (*(1-2e-3)) 0.01)
GNUPlot.plotList [] $ take 101 $ shapeFreqModFromSampledTone Interpolation.linear Interpolation.linear (1/0.07::Double) (iterate (1+) (0::Double)) 0 0 (repeat 1) (repeat 0.7)
-}

shapePhaseFreqModFromSampledTone :: (RealField.C t) =>
    Interpolation.T t y ->
    Interpolation.T t y ->
    t -> Sig.T y -> t -> t -> Sig.T t -> Sig.T t -> Sig.T t -> Sig.T y
shapePhaseFreqModFromSampledTone
      ipLeap ipStep period sampledTone
      shape0 phase shapes phases freqs =
   let periodInt = round period
       marginLeap = Interpolation.margin ipLeap
       marginStep = Interpolation.margin ipStep
   in  map
          (uncurry (ToneMod.interpolateCell ipLeap ipStep) .
           ToneMod.seekCell periodInt period) $
       zipWith (\p -> mapFst (mapSnd (Phase.increment p))) phases $
       ToneMod.oscillatorSuffixes
          marginLeap marginStep
          periodInt period sampledTone
          (shape0, shapes)
          (Phase.fromRepresentative phase, freqs)


{- * Oscillators with specific waveforms -}

{- | impulse train with static frequency -}
staticImpulses :: (RealRing.C a) => a -> a -> Sig.T a
staticImpulses phase = freqModImpulses phase . repeat

{- | impulse train with modulated frequency -}
freqModImpulses :: (RealRing.C a) => a -> Sig.T a -> Sig.T a
freqModImpulses phase =
   Sig.crochetL
      (\freq p0 -> Just $
         let p1 = p0+freq
         in if p1>1
               then (1, fraction p1)
               else (0, p1))
      (fraction phase)

{- | sine oscillator with static frequency -}
staticSine :: (Trans.C a, RealRing.C a) => a -> a -> Sig.T a
staticSine = static Wave.sine

{- | sine oscillator with modulated frequency -}
freqModSine :: (Trans.C a, RealRing.C a) => a -> Sig.T a -> Sig.T a
freqModSine = freqMod Wave.sine

{- | sine oscillator with modulated phase, useful for FM synthesis -}
phaseModSine :: (Trans.C a, RealRing.C a) => a -> Sig.T a -> Sig.T a
phaseModSine = phaseMod Wave.sine

{- | saw tooth oscillator with static frequency -}
staticSaw :: RealRing.C a => a -> a -> Sig.T a
staticSaw = static Wave.saw

{- | saw tooth oscillator with modulated frequency -}
freqModSaw :: RealRing.C a => a -> Sig.T a -> Sig.T a
freqModSaw = freqMod Wave.saw
