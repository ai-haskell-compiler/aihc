{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.Dimensional.RateAmplitude.Instrument where

import qualified Synthesizer.Dimensional.Rate.Oscillator as Osci
import qualified Synthesizer.Dimensional.Rate.Filter     as Filt
import qualified Synthesizer.Dimensional.RateAmplitude.Displacement as Disp
import qualified Synthesizer.Dimensional.RateAmplitude.Noise      as Noise
-- import qualified Synthesizer.SampleRateDimension.Filter.Recursive    as FiltR
-- import qualified Synthesizer.SampleRateDimension.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Dimensional.RateAmplitude.Filter     as FiltA
import qualified Synthesizer.Dimensional.RateAmplitude.Cut        as Cut
import qualified Synthesizer.Dimensional.Amplitude.Cut            as CutA

import qualified Synthesizer.Dimensional.RateAmplitude.Piece      as Piece
import qualified Synthesizer.Dimensional.RateAmplitude.Control    as Ctrl
import qualified Synthesizer.Dimensional.Rate.Control             as CtrlR

import qualified Synthesizer.Dimensional.Amplitude.Analysis       as Ana

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal as SigA

import Synthesizer.Dimensional.Signal (($-), ($&), (&*^), (&*>^), )
import Synthesizer.Dimensional.RateAmplitude.Piece ((-|#), ( #|-), (|#), ( #|), )
import Synthesizer.Dimensional.Wave ((&*~), )

import Synthesizer.Dimensional.Process (($:), ($::), ($^), (.^), ($#), )
import qualified Synthesizer.Dimensional.Amplitude.Displacement as DispA

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Sample as Sample
-- import qualified Synthesizer.Dimensional.Rate as Rate

-- import qualified Synthesizer.Storable.Signal as SigSt
import Foreign.Storable (Storable, )

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import Number.DimensionTerm ((*&), (&*&), )

import qualified Synthesizer.Interpolation.Module as Interpolation
import           Synthesizer.Plain.Instrument (choirWave)
import qualified Synthesizer.Dimensional.Wave.Controlled as WaveCtrl
import qualified Synthesizer.Dimensional.Wave as WaveD
import qualified Synthesizer.Generic.Wave     as WaveG
import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Basic.Phase      as Phase

import qualified Number.NonNegative     as NonNeg

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Ring           as Ring

import System.Random (Random, randoms, randomRs, mkStdGen, )
import Synthesizer.Utility (randomRsBalanced, balanceLevel, )

import Data.List(zip4)

import NumericPrelude.Base
import NumericPrelude.Numeric



{-| Create a sound of a slightly changed frequency
    just as needed for a simple stereo sound. -}
{-# INLINE stereoPhaser #-}
stereoPhaser :: Ring.C a =>
      (DN.T Dim.Frequency a ->
       Proc.T s Dim.Time a (SigA.R s u b b))
           {- ^ A function mapping a frequency to a signal. -}
   -> a    {- ^ The factor to the frequency, should be close to 1. -}
   -> DN.T Dim.Frequency a
           {- ^ The base (undeviated) frequency of the sound. -}
   -> Proc.T s Dim.Time a (SigA.R s u b b)
stereoPhaser sound dif freq =
   sound (dif *& freq)



{-
allpassPlain :: (RealField.C a, Trans.C a, Module.C a a) =>
                   a -> a -> a -> a -> [a]
allpassPlain sampleRate halfLife k freq =
    Filt.allpassCascade 10
        (map Filt.AllpassParam (exponential2 (halfLife*sampleRate) k))
        (simpleSaw sampleRate freq)
-}

{-# INLINE allpassDown #-}
allpassDown ::
   (RealField.C a, Trans.C a, Module.C a a) =>
      NonNeg.Int -> DN.T Dim.Time a ->
      DN.T Dim.Frequency a -> DN.T Dim.Frequency a ->
      Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
allpassDown order halfLife filterfreq freq =
   do x <- simpleSaw freq
      FiltA.amplify 0.3 $:
         (Disp.mix
             $# x
             $: (Filt.allpassCascade order Filt.allpassFlangerPhase
                    $: filterfreq &*^ CtrlR.exponential2 halfLife
                    $# x))


{-# INLINE moogDown #-}
{-# INLINE moogReso #-}
moogDown, moogReso ::
   (RealField.C a, Trans.C a, Module.C a a) =>
      NonNeg.Int -> DN.T Dim.Time a ->
      DN.T Dim.Frequency a -> DN.T Dim.Frequency a ->
      Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
moogDown order halfLife filterfreq freq =
   Filt.moogLowpass order
      $- DN.fromNumber 10
      $: filterfreq &*^ CtrlR.exponential2 halfLife
      $: simpleSaw freq

moogReso order halfLife filterfreq freq =
   Filt.moogLowpass order
      $: DN.fromNumber 100 &*>^ CtrlR.exponential2 halfLife
      $- filterfreq
      $: simpleSaw freq


{-# INLINE bell #-}
bell :: (Trans.C a, RealField.C a, Module.C a a) =>
   DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
bell freq =
   let halfLife = DN.time 0.5
   in  FiltA.amplify (1/3) $:
       (Disp.mixMulti $::
          (bellHarmonic 1 halfLife freq :
           bellHarmonic 4 halfLife freq :
           bellHarmonic 7 halfLife freq :
           []))



{-# INLINE bellHarmonic #-}
bellHarmonic :: (Trans.C a, RealField.C a, Module.C a a) =>
   a -> DN.T Dim.Time a -> DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
bellHarmonic n halfLife freq =
   Filt.envelope
      $: CtrlR.exponential2 (recip n *& halfLife)
      $: (Osci.freqMod (DN.voltage 1 &*~ Wave.sine) zero
            $: Osci.static (WaveD.mapLinear 0.005 (DN.frequency 5) Wave.sine)
                  zero (n *& freq))


{-# INLINE fastBell #-}
{-# INLINE squareBell #-}
{-# INLINE moogGuitar #-}
{-# INLINE moogGuitarSoft #-}
{-# INLINE fatSaw #-}

fastBell, squareBell, moogGuitar, moogGuitarSoft, fatSaw ::
   (RealField.C a, Trans.C a, Module.C a a) =>
   DN.T Dim.Frequency a -> Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
fastBell freq =
   Filt.envelope
      $: CtrlR.exponential2 (DN.time 0.2)
      $: Osci.static (DN.voltage 1 &*~ Wave.sine) zero freq

{-# INLINE filterSaw #-}
filterSaw :: (Module.C a a, Trans.C a, RealField.C a) =>
   DN.T Dim.Frequency a -> DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
filterSaw filterFreq freq =
   FiltA.amplify 0.1 $:
   (Filt.lowpassFromUniversal $^
     (Filt.universal
         $- DN.fromNumber 10
         $: filterFreq &*^ CtrlR.exponential2 (DN.time 0.1)
         $: Osci.static (DN.voltage 1 &*~ Wave.saw) zero freq))


squareBell freq =
   Filt.firstOrderLowpass
      $: DN.frequency 4000 &*^ CtrlR.exponential2 (DN.time (1/10))
--       (Osci.freqModSample Interpolation.cubic [0, 0.7, -0.3, 0.7, 0, -0.7, 0.3, -0.7] zero
      $: (Osci.freqMod
             (sampledWave Interpolation.linear (DN.voltage 1)
                 [0, 0.5, 0.6, 0.8, 0, -0.5, -0.6, -0.8]) zero
             $: (Osci.static (WaveD.mapLinear 0.01 freq Wave.sine) zero (DN.frequency 5.0)))


{-# INLINE fmBell #-}
fmBell :: (RealField.C a, Trans.C a, Module.C a a) =>
   a -> a -> DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
fmBell depth freqRatio freq =
   let modul =
          Filt.envelope
             $: CtrlR.exponential2 (DN.time 0.2)
             $: Osci.static (DN.fromNumber depth &*~ Wave.sine)
                   zero (freqRatio *& freq)
   in  Filt.envelope
          $: CtrlR.exponential2 (DN.time 0.5)
          $: (Osci.phaseMod (DN.voltage 1 &*~ Wave.sine) freq $& modul)


moogGuitar freq =
   let filterControl =
          DN.frequency 4000 &*^ CtrlR.exponential2 (DN.time 0.5)
       tone =
          Osci.freqMod (DN.voltage 1 &*~ Wave.saw) zero
              $: Osci.static (WaveD.mapLinear 0.005 freq Wave.sine)
                    zero (DN.frequency 5)
   in  Filt.moogLowpass 4 $- DN.fromNumber 10 $: filterControl $: tone

moogGuitarSoft freq =
   Filt.envelope
      $: (DispA.map (1-) $^ CtrlR.exponential2 (DN.time 0.003))
      $: moogGuitar freq


{- |
Phase modulation using a ring modulated signal.
May be used as some kind of e-guitar.
-}
fmRing ::
   (RealField.C a, Trans.C a, Module.C a a) =>
   DN.T Dim.Frequency a -> Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
fmRing freq =
   Osci.phaseMod (DN.voltage 1 &*~ Wave.sineSawSmooth 1) freq
     $: (Filt.envelope
           $: CtrlR.exponential2 (DN.time 0.2)
           $: (Filt.envelope
                  $: Osci.static (WaveD.flat $ Wave.raise one Wave.sine) (Phase.fromRepresentative 0.75) freq
                  $: Osci.static
                        (DN.fromNumber 0.2 &*~  {- 0.2 for no distortion -} Wave.sine)
                        zero (5.001 *& freq)))

fatPad ::
   (RealField.C a, Trans.C a, Module.C a a, Random a) =>
   DN.T Dim.Frequency a -> Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
fatPad freq =
   let env =
          Cut.append
             $: (Cut.take (DN.time 0.7) $:
                  Ctrl.cubicHermite
                   (DN.time 0,   (DN.fromNumber 0,   DN.frequency 1 &*& DN.fromNumber 5))
                   (DN.time 0.7, (DN.fromNumber 0.5, DN.frequency 1 &*& DN.fromNumber 0)))
             $: Ctrl.constant (DN.fromNumber 0.5)
       osci f =
          Osci.phaseMod (DN.voltage 0.3 &*~ Wave.sine) f
            $: (Filt.envelope
                   $: env
                   $: Osci.static (DN.fromNumber 2 &*~ Wave.sineSawSmooth 1) zero f)
       freqs = randomRsBalanced (mkStdGen 384) 3 1 0.03
   in  Disp.mixMulti $:: map (\k -> osci (k *& freq)) freqs
{-
renderTimeVoltageMonoDoubleToInt16 (DN.frequency 44100) "fat-pad" (Cut.take (DN.time 1.5) $: fatPad (DN.frequency 220))
-}


brass ::
   (RealField.C a, Trans.C a, Module.C a a, Random a) =>
   DN.T Dim.Frequency a -> Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
brass freq =
   let blobEnv = Piece.runState
          (DN.fromNumber 0  |# (DN.time 0.05, Piece.cosine) #|-
           DN.fromNumber 1 -|# (DN.time 0.05, Piece.cosine) #|
           DN.fromNumber 0)
       adsr = Piece.runState
          (DN.fromNumber 0 |# (DN.time 0.1, Piece.cubic (DN.frequency 1 &*& DN.fromNumber 10) (DN.frequency 1 &*& DN.fromNumber 0)) #|-
           DN.fromNumber 0.5 -|# (DN.time 1, Piece.step) #|-
           DN.fromNumber 0.5 -|# (DN.time 0.3, Piece.exponential (DN.fromNumber 0)) #|
           DN.fromNumber 0.01)
       osci b f =
          Osci.freqMod (DN.voltage 0.5 &*~ Wave.saw) zero $:
             (Disp.mix
                 $: (Osci.static (WaveD.mapLinear 0.01 f Wave.sine)
                        zero (DN.frequency 2))
                 $: ((b *& f) &*^ blobEnv))
       n = 4
       freqs = randomRsBalanced (mkStdGen 295) n 1 0.03
       blobAmps = balanceLevel 0 (take n (iterate (0.1+) 0))
   in  Filt.envelope
          $: adsr
          $: (Disp.mixMulti $:: zipWith (\b k -> osci b (k *& freq)) blobAmps freqs)
{-
Synthesizer.Dimensional.RateAmplitude.File.renderTimeVoltageMonoDoubleToInt16 (DN.frequency 44100) "brass.aiff" (brass (DN.frequency 440))
-}


{-| low pass with resonance -}
{-# INLINE filterSweep #-}
filterSweep :: (Module.C a v, Trans.C a, RealField.C a) =>
   Phase.T a ->
   Proc.T s Dim.Time a (
      SigA.R s Dim.Voltage a v ->
      SigA.R s Dim.Voltage a v)
filterSweep phase =
   Filt.lowpassFromUniversal .^
    (Filt.universal
       $- DN.fromNumber 10
       $: Osci.static (WaveD.mapExponential 2 (DN.frequency 1800) Wave.sine)
             phase (DN.frequency (1/16)))


{-# INLINE fatSawChordFilter #-}
{-# INLINE fatSawChord #-}
fatSawChordFilter, fatSawChord ::
   (RealField.C a, Trans.C a, Module.C a a) =>
   DN.T Dim.Frequency a -> Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)

fatSawChordFilter freq =
   FiltA.amplify (1/2) $:
   (Filt.lowpassFromUniversal $^
     (Filt.universal
         $- DN.fromNumber 10
         $: filterDown
         $: fatSawChord freq))

fatSawChord freq =
   FiltA.amplify (1/3) $:
   (Disp.mixMulti $::
       [fatSaw ( 1    *& freq),
        fatSaw ((5/4) *& freq),
        fatSaw ((3/2) *& freq)])

{-# INLINE filterDown #-}
filterDown :: (RealField.C a, Trans.C a) =>
   Proc.T s Dim.Time a (SigA.R s Dim.Frequency a a)
filterDown =
   DN.frequency 4000 &*^ CtrlR.exponential2 (DN.time (1/3))

{-# INLINE simpleSaw #-}
simpleSaw :: (Ring.C a, Dim.C u, RealField.C v) =>
   DN.T (Dim.Recip u) v ->
   Proc.T s u v (SigA.R s Dim.Voltage a v)
simpleSaw freq =
   Osci.static (DN.voltage 1 &*~ Wave.saw) zero freq


{-| accumulate multiple similar saw sounds and observe the increase of volume
    The oscillator @osc@ must accept relative frequencies. -}
{-# INLINE modulatedWave #-}
modulatedWave :: (Trans.C a, RealField.C a, Dim.C u) =>
   Proc.T s u a (SigA.R s (Dim.Recip u) a a -> SigA.R s Dim.Voltage a a) ->
   DN.T (Dim.Recip u) a ->
   a -> Phase.T a ->
   DN.T (Dim.Recip u) a ->
   Proc.T s u a (SigA.R s Dim.Voltage a a)
modulatedWave osc freq depth phase speed =
   osc $: Osci.static (WaveD.mapLinear depth freq Wave.sine) phase speed


{-# INLINE accumulationParameters #-}
accumulationParameters :: (Random a, Trans.C a, RealField.C a, Module.C a a) =>
   [(Phase.T a, a, Phase.T a, DN.T Dim.Frequency a)]
accumulationParameters =
   let starts = randoms           (mkStdGen 48251)
       depths = randomRs (0,0.02) (mkStdGen 12354)
       phases = randoms           (mkStdGen 74389)
       speeds = randomRs (DN.frequency 0.1, DN.frequency 0.3)
                                  (mkStdGen 03445)
   in  zip4 starts depths phases speeds

{-# INLINE accumulatedSaws #-}
{-# INLINE choir #-}
accumulatedSaws, choir ::
   (Random a, Trans.C a, RealField.C a, Module.C a a) =>
   DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
accumulatedSaws freq =
    Disp.mixMulti $::
       (map
          (\(start, depth, phase, speed) ->
               modulatedWave
                  (Osci.freqMod (DN.voltage 1 &*~ Wave.saw) start)
                  freq depth phase speed)
          accumulationParameters)

choir freq =
   FiltA.amplify 0.2 $: (Disp.mixMulti $::
      take 10
         (map
            (\(start, depth, phase, speed) ->
                modulatedWave
                  (Osci.freqMod
                      (sampledWave Interpolation.constant (DN.voltage 1) choirWave) start)
                  freq depth phase speed)
            accumulationParameters))


fatSaw freq =
    {- a simplified version of modulatedWave -}
    let partial depth modPhase modFreq =
           osciDoubleSaw $:
              Osci.static (WaveD.mapLinear depth freq Wave.sine)
                 (Phase.fromRepresentative modPhase) modFreq
    in  Disp.mixMulti $::
            [partial 0.00311 0.0 (DN.frequency 20),
             partial 0.00532 0.3 (DN.frequency 17),
             partial 0.00981 0.9 (DN.frequency  6)]


{-# INLINE wasp #-}
{- |
A good choice is @freq = DN.frequency 110@
-}
wasp ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Dim.C u) =>
   DN.T (Dim.Recip u) q ->
   Proc.T s u q (SigA.R s Dim.Voltage q q)
wasp freq =
   Filt.envelope
      $: Osci.static (WaveD.mapLinear 1 (DN.scalar 0.5) Wave.saw)
           zero (recip 2.01 *& freq)
      $: Osci.static (DN.voltage 0.7 &*~ Wave.saw) zero freq


{-# INLINE osciDoubleSaw #-}
osciDoubleSaw ::
   (RealField.C a, Module.C a a, Dim.C u) =>
   Proc.T s u a (
      SigA.R s (Dim.Recip u) a a ->
      SigA.R s Dim.Voltage a a)
osciDoubleSaw =
   Osci.freqMod
      (sampledWave Interpolation.linear (DN.voltage 1)
          [-1, -0.2, 0.5, -0.5, 0.2, 1.0]) zero

{-
sampledWave :: (RealField.C t, Storable y) =>
   Interpolation.T t y -> amp -> [y] ->
   WaveD.T t (Sample.Numeric amp y)
sampledWave ip amp =
   WaveD.amplified amp . WaveG.sample ip .
   SigSt.fromList SigSt.defaultChunkSize
-}

sampledWave :: (RealField.C t) =>
   Interpolation.T t y -> amp -> [y] ->
   WaveD.T t (Sample.Numeric amp y)
sampledWave ip amp =
   WaveD.amplified amp . WaveG.sample ip

{-|
A tone with a waveform with roughly the dependency @x -> x^?p@,
where the waveform is normalized to constant quadratic norm
-}
{-# INLINE osciSharp #-}
osciSharp :: (RealField.C a, Trans.C a) =>
   DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
osciSharp freq =
   Osci.shapeMod (DN.voltage 1 `WaveCtrl.amplified` Wave.powerNormed2) zero freq $:
   (Flat.canonicalize $^
    DN.fromNumber 10 &*^ CtrlR.exponential2 (DN.time 0.01))

{-|
Build a saw sound from its harmonics and modulate it.
Different to normal modulation
I modulate each harmonic with the same depth rather than a proportional one.
-}
{-# INLINE osciAbsModSaw #-}
osciAbsModSaw :: (RealField.C a, Trans.C a, Module.C a a) =>
   DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
osciAbsModSaw freq =
   let harmonic n =
          Osci.freqMod (DN.voltage (0.25 / fromInteger n) &*~ Wave.sine) zero
             $: Osci.static (WaveD.mapLinear 0.03 freq Wave.sine) zero (DN.frequency 1)
   in  Disp.mixMulti $:: map harmonic [1..20]

{-|
Short pulsed Noise.white,
i.e. Noise.white amplified with pulses of varying H\/L ratio.
-}
{-# INLINE pulsedNoise #-}
pulsedNoise :: (Random a, RealField.C a, Trans.C a, Module.C a a) =>
   DN.T Dim.Frequency a   {-^ frequency of the pulses, interesting ones are around 100 Hz and below -} ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
pulsedNoise freq =
   let raisedSine :: Trans.C a => a -> WaveD.T a (Sample.Dimensional Dim.Voltage a a)
       raisedSine v = DN.voltage v &*~ Wave.raise one Wave.sine
       c = Proc.pure Ana.lessOrEqual
              $: Osci.static (raisedSine 1.0) zero freq
              $: Osci.static (raisedSine 0.2) zero (DN.frequency 0.1)
   in  Proc.pure CutA.selectBool
          $- DN.voltage 0
          $: Noise.white (DN.frequency 20000) (DN.voltage 1.0)
          $: c


{-# INLINE noisePerc #-}
noisePerc :: (Random a, RealField.C a, Trans.C a) =>
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
noisePerc =
   Filt.envelope
      $: CtrlR.exponential2 (DN.time 0.1)
      $: Noise.white (DN.frequency 20000) (DN.voltage 1.0)

{-# INLINE noiseBass #-}
noiseBass :: (Random a, RealField.C a, Trans.C a, Module.C a a, Storable a) =>
   DN.T Dim.Frequency a ->
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
noiseBass freq =
   FiltA.combProc (DN.unrecip freq)
      (Filt.firstOrderLowpass $- DN.frequency 2000)
      $: noisePerc

{-|
Drum sound using the Karplus-Strong-Algorithm
This is a Noise.white enveloped by an exponential2
which is piped through the Karplus-Strong machine
for generating some frequency.
The whole thing is then frequency modulated
to give a falling frequency.
-}
{-# INLINE electroTom #-}
electroTom ::
   (Random a, RealField.C a, Trans.C a, Module.C a a, Storable a) =>
   Proc.T s Dim.Time a (SigA.R s Dim.Voltage a a)
electroTom =
   let ks =
         FiltA.combProc (DN.time (1/30))
            (Filt.firstOrderLowpass $- (DN.frequency 1000))
            $: noisePerc
   in  Filt.frequencyModulation Interpolation.linear
          $: CtrlR.exponential2 (DN.time 0.3)
          $: ks

{-# INLINE bassDrum #-}
bassDrum ::
   (RealField.C q, Trans.C q, Module.C q q, Random q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
bassDrum =
   Cut.take (DN.time 0.15) $:
   (Disp.mix
    $: (Filt.firstOrderLowpass
          $- (DN.frequency 5000)
          $: (Filt.envelope
                $: (DispA.map (0.03+) $^ CtrlR.exponential2 (DN.time 0.002))
                $: (Noise.white (DN.frequency 20000) (DN.voltage 1))))
    $: (Filt.envelope
          $: (CtrlR.exponential2 (DN.time 0.05))
          $: (Osci.freqMod (DN.voltage 0.5 &*~ Wave.sine) zero
                 $: (Ctrl.exponential2
                       (DN.time 0.15) (DN.frequency 100)))))
