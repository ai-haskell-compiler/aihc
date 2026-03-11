{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Dimensional.RateAmplitude.Demonstration where

import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Map as MapD
import qualified Synthesizer.Dimensional.Rate.Oscillator as Osci
import qualified Synthesizer.Dimensional.Rate.Filter     as Filt
import qualified Synthesizer.Dimensional.RateAmplitude.Displacement as Disp
import qualified Synthesizer.Dimensional.RateAmplitude.Noise      as Noise
import qualified Synthesizer.Dimensional.RateAmplitude.Filter     as FiltA
import qualified Synthesizer.Dimensional.RateAmplitude.Cut        as Cut
import qualified Synthesizer.Dimensional.Rate.Cut                 as CutR

import qualified Synthesizer.Dimensional.RateAmplitude.Control    as Ctrl
import qualified Synthesizer.Dimensional.Rate.Control             as CtrlR

import qualified Synthesizer.Dimensional.Amplitude.Displacement   as DispA

import qualified Synthesizer.Dimensional.Causal.FilterParameter   as FiltCP
import qualified Synthesizer.Dimensional.Causal.Displacement      as DispC
import qualified Synthesizer.Dimensional.Causal.Process           as CausalD
import qualified Synthesizer.Dimensional.Causal.ControlledProcess as CProc

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal as SigA

import qualified Synthesizer.Dimensional.RateAmplitude.File as File
-- import qualified Synthesizer.Dimensional.RateAmplitude.Play as Play

import Synthesizer.Dimensional.Causal.Process (($/:))
import Synthesizer.Dimensional.Signal (($-), (&*^), )
import Synthesizer.Dimensional.Process (($:), ($::), ($^), )
import Synthesizer.Dimensional.RateAmplitude.Instrument (wasp, )

import qualified Synthesizer.Dimensional.Wave as WaveD
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate
import Synthesizer.Dimensional.Wave ((&*~), )

import qualified Synthesizer.Frame.Stereo as Stereo
import Foreign.Storable (Storable, )

import qualified Synthesizer.Interpolation.Custom as Interpolation
import qualified Synthesizer.Interpolation.Module as IpMod
import qualified Synthesizer.Interpolation.Class  as Interpol
-- import qualified Synthesizer.Basic.WaveSmoothed as WaveSmooth
import qualified Synthesizer.Basic.Wave         as Wave
import qualified Synthesizer.Basic.Phase        as Phase

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import Number.DimensionTerm ((*&))

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Ring           as Ring

import System.Time (getClockTime, diffClockTimes, tdSec, tdPicosec, )
import System.IO (hFlush, stdout, )
import System.Exit (ExitCode(ExitFailure), exitWith, )
import qualified Control.Monad.Exception.Synchronous as Exc

import System.Random (Random, randomRs, mkStdGen, )

import Control.Arrow ((<<<), )
import Control.Applicative (liftA2, )

import Data.Tuple.HT (snd3, )

import NumericPrelude.Base
import NumericPrelude.Numeric




{-# INLINE sineLow #-}
sineLow ::
   (RealField.C q, Trans.C q, Module.C q q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
sineLow =
   Osci.static (DN.voltage 1 &*~ Wave.sine) zero (DN.frequency 440)

{-# INLINE sineHigh #-}
sineHigh ::
   (RealField.C q, Trans.C q, Module.C q q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
sineHigh =
   Osci.static (DN.voltage 1 &*~ Wave.sine) zero (DN.frequency 660)

{-# INLINE sineMix #-}
sineMix ::
   (RealField.C q, Trans.C q, Module.C q q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
sineMix =
   FiltA.amplify 0.5 $: (Disp.mix $: sineLow $: sineHigh)


{-# INLINE exponential #-}
exponential ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.T (Rate.Phantom s) (Amp.Flat q) (Sig.T q))
exponential =
   CtrlR.exponential (DN.time 0.3)


{-# INLINE ping #-}
ping ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
ping =
   Filt.envelope
      $: exponential
      $: sineLow

{-# INLINE pingSaw #-}
pingSaw ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
pingSaw =
   Filt.envelope
      $: exponential
      $: saw



{-# INLINE sawWave #-}
sawWave :: (RealField.C a) => Wave.T a a
sawWave = Wave.triangleAsymmetric (-0.9)

{-# INLINE saw #-}
saw ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
saw =
   Osci.static (DN.voltage 1 &*~ sawWave) zero (DN.frequency 440)

{-# INLINE sawVibrato #-}
sawVibrato ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
sawVibrato =
   Osci.freqMod (DN.voltage 1 &*~ sawWave) zero
      $: (Osci.static
             (WaveD.mapLinear 0.01 (DN.frequency 440) Wave.sine)
             zero (DN.frequency 5))

{-# INLINE sawChorus #-}
sawChorus ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
sawChorus =
   let w = DN.voltage (1/4) &*~ sawWave
   in  Disp.mixMulti
         $:: (Osci.static w (Phase.fromRepresentative 0.00) (DN.frequency 442.0) :
              Osci.static w (Phase.fromRepresentative 0.25) (DN.frequency 441.2) :
              Osci.static w (Phase.fromRepresentative 0.50) (DN.frequency 438.7) :
              Osci.static w (Phase.fromRepresentative 0.75) (DN.frequency 438.1) :
              [])




{-# INLINE amplitudeModulationChirp #-}
amplitudeModulationChirp ::
   (RealField.C q, Trans.C q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
amplitudeModulationChirp =
   Filt.envelope
      $: (Osci.static (WaveD.flat Wave.sine) zero (DN.frequency 440))
      $: (Osci.freqMod (DN.voltage 1 &*~ Wave.sine) zero
             $: (Ctrl.exponentialFromTo
                   (DN.time 10)
                   (DN.frequency 1, DN.frequency 1000)))


{-# INLINE airplane #-}
airplane ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
airplane =
   SigA.share
      (Noise.white (DN.frequency 20000) (DN.voltage 0.2))
      (\noise ->
          Cut.take (DN.time 5) $: (Disp.mix
             $: noise
             $: (Filt.frequencyModulation IpMod.linear
                    $- DN.scalar 1.001
                    $: noise)))

{-# INLINE airplaneFade #-}
airplaneFade ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
airplaneFade =
   Filt.envelope
      $: (DispA.map (\t -> recip (1 + 30*(t-1)^2)) $^ CtrlR.linear (DN.time 5))
--      $: Osci.static Wave.sine zero (DN.recip (DN.time 20))
      $: (Filt.phaser Interpolation.linear (DN.time 0.01)
            $: Ctrl.exponentialFromTo
                  (DN.time 10)
                  (DN.unrecip (DN.frequency 5000), DN.unrecip (DN.frequency 100))
            $: Noise.white (DN.frequency 20000) (DN.voltage 0.5))


{-# INLINE wind #-}
wind ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
wind =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 20
         $: (DispA.mapExponential 2 (DN.frequency 1000) $^
               (Disp.mix
                   $: Osci.static (DN.scalar 0.5 &*~ Wave.sine) zero (DN.frequency 0.2)
                   $: Osci.static (DN.scalar 1.0 &*~ Wave.sine) zero (DN.frequency (sqrt 0.2))))
         $: Noise.white (DN.frequency 20000) (DN.voltage 0.1))

{-# INLINE windStereo #-}
windStereo ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q (Stereo.T q))
windStereo =
   SigA.share
      wind
      (\w -> Cut.mergeStereo $: w $: (Cut.drop (DN.time 0.2) $: w))



{-# INLINE sweepFrequency #-}
sweepFrequency ::
   (Trans.C q, RealField.C q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Frequency q q)
sweepFrequency =
   Osci.static
      (WaveD.mapExponential 2 (DN.frequency 1000) Wave.sine)
      zero (DN.frequency 0.2)

{-# INLINE deepSaw #-}
deepSaw ::
   (RealField.C q) =>
   DN.T v q ->
   Proc.T s Dim.Time q (SigA.R s v q q)
deepSaw v =
   Osci.static (v &*~ Wave.saw) zero (DN.frequency 110)

{-# INLINE universalLowpassDirect #-}
universalLowpassDirect ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
universalLowpassDirect =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 20
         $: sweepFrequency
         $: deepSaw (DN.voltage 0.2))

{-# INLINE universalLowpassSync #-}
universalLowpassSync ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
universalLowpassSync =
   Filt.lowpassFromUniversal $^
      (CProc.runSynchronous2 FiltCP.universal
         $- DN.scalar 20
         $: sweepFrequency
         $/: deepSaw (DN.voltage 0.2))

{-# INLINE universalLowpassAsyncLinear #-}
universalLowpassAsyncLinear ::
   (RealField.C q, Trans.C q, Module.C q q, Interpol.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
universalLowpassAsyncLinear =
   Filt.lowpassFromUniversal $^
      (CProc.processAsynchronousBuffered2
         Interpolation.linear FiltCP.universal
         (DN.frequency 10)
--         (Rate.fromNumber Dim.frequency 100)
         (Ctrl.constant (DN.scalar 20))
         sweepFrequency
         $/: deepSaw (DN.voltage 0.2))

{-# INLINE universalLowpassAsyncConstant #-}
universalLowpassAsyncConstant ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
universalLowpassAsyncConstant =
   Filt.lowpassFromUniversal $^
      (CProc.processAsynchronousBuffered2
         Interpolation.constant FiltCP.universal
         (DN.frequency 100)
--         (Rate.fromNumber Dim.frequency 100)
         (Ctrl.constant (DN.scalar 20))
         sweepFrequency
         $/: deepSaw (DN.voltage 0.2))


{-# INLINE allpassPhaserDirect #-}
allpassPhaserDirect ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
allpassPhaserDirect =
   let tone = deepSaw (DN.voltage 0.5)
   in  Disp.mix
          $: (Filt.allpassCascade 20 Filt.allpassFlangerPhase
                $: sweepFrequency
                $: tone)
          $: tone

{-# INLINE allpassPhaserCausal #-}
allpassPhaserCausal ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
allpassPhaserCausal =
   let tone = deepSaw (DN.voltage 0.5)
       phaser =
          do mix     <- DispC.mix
             apcCtrl <-
                liftA2 (<<<)
                   CProc.process
                   (fmap CausalD.first $
                    FiltCP.allpassCascade 20 FiltCP.allpassFlangerPhase)
             ctrl    <- sweepFrequency
             return $
                mix <<<
                CausalD.fanout CausalD.id (CausalD.applyFst apcCtrl ctrl)
   in  phaser $/: tone


{-# INLINE moogSawDirect #-}
moogSawDirect ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
moogSawDirect =
   Filt.moogLowpass 10
      $- DN.scalar 20
      $: sweepFrequency
      $: deepSaw (DN.voltage 0.2)

{-# INLINE moogSawCausal #-}
moogSawCausal ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
moogSawCausal =
   CProc.runSynchronous2 (FiltCP.moogLowpass 10)
      $- DN.scalar 20
      $: sweepFrequency
      $/: deepSaw (DN.voltage 0.2)



{- |
We do not create noise at a low sampling and resample it by intention.
Resampling is intended for maintaining maximum quality
and not for relying on the bad quality of constant interpolation.
Instead we generate a piecewise constant function manually.
-}
{-# INLINE glissandoControl #-}
glissandoControl ::
   (RealField.C q, Trans.C q, Module.C q q, Random q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Scalar q q)
glissandoControl =
   Filt.firstOrderLowpass
      $- DN.frequency 4
      $: (Cut.concatVolume (DN.scalar 1) $:
          mapM (\p ->
             Cut.take (DN.time (1/6))
              $: Ctrl.constant (DN.scalar (fromInteger p / 12)))
              (randomRs (0,24) (mkStdGen 3141)))


{-# INLINE bassFilter #-}
bassFilter ::
   (RealField.C q, Trans.C q, Module.C q q, Random q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q (Stereo.T q))
bassFilter =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 5
{-
         $- DN.frequency 440
-}
         $: (DispA.mapExponential 2 (DN.frequency 440) $^
               glissandoControl)
{-
         $: (DispA.mapExponential 10 (DN.frequency 440) $^
               Osci.static Wave.sine zero (DN.frequency 0.2))
-}
         $: (Cut.mergeStereo
               $: Osci.static (DN.voltage 1 &*~ Wave.saw) zero (DN.frequency 55.0)
               $: Osci.static (DN.voltage 1 &*~ Wave.saw) zero (DN.frequency 55.1)))



{-# INLINE noiseLowpass #-}
noiseLowpass ::
   (RealField.C q, Trans.C q, Module.C q q, Random q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
noiseLowpass =
   let noise = Noise.white (DN.frequency 20000) (DN.voltage 0.1)
       control =
          Ctrl.exponentialFromTo
            (DN.time 5)
            (DN.frequency 10000, DN.frequency 10)
   in  Filt.firstOrderLowpass
          $: control
          $: noise


{-# INLINE noiseHighpass #-}
noiseHighpass ::
   (RealField.C q, Trans.C q, Module.C q q, Random q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
noiseHighpass =
   let noise = Noise.white (DN.frequency 20000) (DN.voltage 0.1)
       control =
          Ctrl.exponentialFromTo
            (DN.time 5)
            (DN.frequency 10000, DN.frequency 10)
   in  Filt.firstOrderHighpass
          $: control
          $: noise


{-# INLINE bubbles #-}
bubbles ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
bubbles =
   let delay = 0.24
   in  Filt.comb (DN.time delay) (0.5 `asTypeOf` delay) $:
       (Osci.freqMod (DN.voltage 0.5 &*~ Wave.sine) zero $:
         (DispA.mapExponential 0.5 (DN.frequency 440) $^
            (Disp.mix
               $: Osci.static (DN.scalar 1.5 &*~ Wave.saw) zero (DN.frequency 0.5)
               $: Osci.static (DN.scalar 0.5 &*~ Wave.saw) zero (DN.frequency 10))))


{-# INLINE bubblesStereo #-}
bubblesStereo ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q (Stereo.T q))
bubblesStereo =
   let delay = 0.24
       {-# INLINE channel #-}
       channel f =
          Osci.freqMod (DN.voltage 0.5 &*~ Wave.sine) zero $:
            (DispA.mapExponential 0.5 (DN.frequency 440) $^
               (Disp.mix
                  $: Osci.static (DN.scalar 1.5 &*~ Wave.saw) zero (DN.frequency 0.5)
                  $: Osci.static (DN.scalar 0.5 &*~ Wave.saw) zero f))
   in  Filt.comb (DN.time delay) (0.5 `asTypeOf` delay) $:
          (Cut.mergeStereo
              $: channel (DN.frequency 10)
              $: channel (DN.frequency 9.23))


{-# INLINE dampedEcho #-}
dampedEcho ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
dampedEcho =
   FiltA.combProc (DN.time 0.2)
            (Filt.firstOrderLowpass $- DN.frequency 1000)
      $: (Filt.envelope
            $: CtrlR.exponential2 (DN.time 0.1)
            $: Osci.static (DN.voltage 1 &*~ Wave.saw) zero (DN.frequency 440))


{-# INLINE trapezoid #-}
trapezoid ::
   (RealField.C q, Trans.C q, Module.C q q, Random q, Storable q) =>
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
trapezoid =
   Filt.mean (DN.frequency 500)
      $: Osci.static (WaveD.mapExponential 4 (DN.frequency 2000) Wave.sine)
            zero (DN.frequency 1)
      $: Osci.static (DN.voltage 0.7 &*~ Wave.trapezoid 0.9)
            zero (DN.frequency 440)
{-
   Filt.meanStatic (DN.frequency 440)
      $: Osci.static (DN.voltage 1 &*~ Wave.square) zero (DN.frequency 440)
-}



{-# INLINE staticSine #-}
staticSine ::
   (RealField.C q, Trans.C q) =>
   Proc.T s Dim.Time q (SigA.T (Rate.Phantom s) (Amp.Flat q) (Sig.T q))
staticSine =
   CutR.take (DN.time 10)
      $: (Osci.static (WaveD.flat Wave.sine) zero (DN.frequency 440))


{-# INLINE harmonicTone #-}
harmonicTone ::
   (RealField.C q, Trans.C q, Module.C q q) =>
   [(DN.Frequency q, q, Phase.T q)] ->
   Proc.T s Dim.Time q (SigA.R s Dim.Voltage q q)
harmonicTone hs =
   let k = recip (sum (map (abs . snd3) hs))
   in  Disp.mixMulti $::
          map (\(f, amp, phase) ->
                  Osci.static (DN.voltage (amp*k) &*~ Wave.sine) phase f) hs

newtype Sound q v =
   Sound {fromSound :: forall s. Proc.T s Dim.Time q (SigA.R s Dim.Voltage q v)}

{-# INLINE harmonicExamples #-}
harmonicExamples ::
   (RealField.C q, Trans.C q, Module.C q q) =>
   [(FilePath, Sound q q)]
harmonicExamples =
   do expo <- [0,1,2]
      (harmName,harm)
           <- [("all", take 10 [1 ..]), ("odd", take 10 [1,3 ..])]
      (phaseName,phase)
           <- [("sin", Phase.fromRepresentative 0),
               ("cos", Phase.fromRepresentative (1/4))]
      return
         ("power" ++ show expo ++ harmName ++ "-" ++ phaseName,
          Sound
             (harmonicTone
                (map ((\n -> (n *& DN.frequency 440,
                             recip (n ^ expo),
                             phase))
                      . fromIntegral)
                     (harm::[Int]))))

{- |
Morphing shapes with constant sound.
By shifting the frequency of all harmonics up by an constant amount,
the periods of the harmonic do no longer match
and recombine only afte a period that depends on the frequency shift.
At the beginning we have the waveform of mixed sines,
after a quarter period of the shift frequency we have mixed cosines and so on.
-}
{-# INLINE harmonicMorph #-}
harmonicMorph ::
   (RealField.C q, Trans.C q, Module.C q q) =>
   [(FilePath, Sound q q)]
harmonicMorph =
   do expo <- [0,1,2]
      (harmName,harm)
           <- [("all", take 10 [1 ..]), ("odd", take 10 [1,3 ..])]
      return
         ("power" ++ show expo ++ harmName ++ "-shift",
          Sound
             (harmonicTone
                (map ((\n -> (n *& DN.frequency 440 + DN.frequency 1,
                             recip (n ^ expo),
                             zero))
                      . fromIntegral)
                     (harm::[Int]))))


{-# INLINE waveforms #-}
waveforms ::
   (RealField.C q, Trans.C q, Module.C q q) =>
   [(FilePath, Sound q q)]
waveforms =
   do (name,wave)
           <- ("square",   Wave.trapezoid 0.9) :
              ("triangle", Wave.triangle) :
              ("saw",      sawWave) :
              []
      return
         (name,
          Sound
             (Osci.static (DN.voltage 1 &*~ wave) zero (DN.frequency 440)))


{- ToDo: reactivate that
{-# INLINE waveformsBandlimited #-}
waveformsBandlimited ::
   (RealField.C q, Trans.C q, Module.C q q) =>
   [(FilePath, Sound q q)]
waveformsBandlimited =
   do (name,wave)
           <- ("square",   WaveSmooth.square) :
              ("triangle", WaveSmooth.triangle) :
              ("saw",      WaveSmooth.saw) :
              ("sine",     WaveSmooth.sine) :
              ("harmonic", WaveSmooth.composedHarmonics $
                  let k = 0.5
                  in  [WaveSmooth.harmonic zero 0,
                       WaveSmooth.harmonic zero k,
                       WaveSmooth.harmonic zero (k/2),
                       WaveSmooth.harmonic zero (k/3),
                       WaveSmooth.harmonic zero (k/4)]) :
              []
      return
         (name++"-antialias-chirp",
          Sound
             ((Osci.freqModAntiAlias (DN.voltage 1 &*~ wave) zero $:
                 Ctrl.line (DN.time 10) (DN.frequency (-30000), DN.frequency 30000))))
-}


measureTime :: String -> IO a -> IO a
measureTime name act =
   do putStr (name++": ")
      hFlush stdout
      timeA <- getClockTime
      res <- act
      timeB <- getClockTime
      let td = diffClockTimes timeB timeA
      print (fromIntegral (tdSec td) +
             fromInteger (tdPicosec td) * 1e-12 :: Double)
      return res

renderToAIFF :: (Ring.C a) =>
   (DN.Frequency a -> String ->
    (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double v)) -> IO ExitCode) ->
   String ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double v)) ->
   Exc.ExceptionalT Int IO ()
renderToAIFF render name sound =
   Exc.fromExitCodeT $
   measureTime name $
   render (DN.frequency 44100) (name++".aiff") sound

renderPrefix ::
   String -> String ->
   (forall s. SigA.R s Dim.Voltage Double v -> SigA.R s Dim.Voltage Double Double) ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double v)) ->
   Exc.ExceptionalT Int IO ()
renderPrefix name ext filterSelect sound =
   let subName = name ++ "-" ++ ext
   in  renderToAIFF
       File.renderTimeVoltageMonoDoubleToInt16
       subName
       (Cut.take (DN.time 10) $: fmap filterSelect sound)

renderFilter ::
   (Interpol.C Double param,
    CProc.C global param
       (Sample.T (Amp.Dimensional Dim.Voltage Double) Double)
       (Sample.T (Amp.Dimensional Dim.Voltage Double) v)) =>
   Double ->
   (forall s.
      SigA.R s Dim.Voltage Double v ->
      SigA.R s Dim.Voltage Double Double) ->
   (forall s. Proc.T s Dim.Time Double
      (-- SigS.R s Double ->
       SigA.R s Dim.Scalar Double Double ->
       SigA.R s Dim.Frequency Double Double ->
       SigA.R s Dim.Voltage Double Double ->
       SigA.R s Dim.Voltage Double v)) ->
   (forall s.
      Proc.T s Dim.Time Double
         (MapD.T
            (Sample.Dimensional Dim.Scalar Double Double,
             Sample.Dimensional Dim.Frequency Double Double)
            (Sample.T global (CProc.RateDep s param)))) ->
   String ->
   Exc.ExceptionalT Int IO ()
renderFilter filterResonance filterSelect filterDirect filterCausal name = do
   renderPrefix name "direct" filterSelect
      (filterDirect
         $- DN.scalar filterResonance
         $: sweepFrequency
         $: deepSaw (DN.voltage 1))
   renderPrefix name "sync" filterSelect
      (CProc.runSynchronous2 filterCausal
         $- DN.scalar filterResonance
         $: sweepFrequency
         $/: deepSaw (DN.voltage 1))
   renderPrefix name "async-constant" filterSelect
      (CProc.processAsynchronousBuffered2
         Interpolation.constant filterCausal
         (DN.frequency 100)
         (Ctrl.constant (DN.scalar filterResonance))
         sweepFrequency
         $/: deepSaw (DN.voltage 1))
   renderPrefix name "async-linear" filterSelect
      (CProc.processAsynchronousBuffered2
         Interpolation.linear filterCausal
         (DN.frequency 10)
         (Ctrl.constant (DN.scalar filterResonance))
         sweepFrequency
         $/: deepSaw (DN.voltage 1))



main :: IO ()
main = Exc.resolveT (exitWith . ExitFailure) $
   do
{-
      Play.timeVoltageMonoDoubleR (DN.frequency 44100) bubbles
-}
{-
      File.writeTimeVoltage "chirp"
         (SigA.render
             (DN.frequency (44100::Double))
             amplitudeModulationChirp)
-}
      mapM_
         (\(name, sound) ->
             renderToAIFF
             File.renderTimeVoltageStereoDoubleToInt16
             name (fromSound sound)) $

         ("bass-filter", Sound (Cut.take (DN.time 15) $: bassFilter)) :
         ("wind",        Sound (Cut.take (DN.time 10) $: windStereo)) :
         ("bubbles",     Sound (Cut.take (DN.time 10) $: bubblesStereo)) :
         []

      mapM_
         (\(name, filt) -> filt name) $
         ("allpass-phaser",
              renderFilter 0.5
                 id
--                 (Filt.allpassPhaser 10)
                 (fmap (\p q f -> CausalD.apply (p q f)) $
                  CProc.runSynchronous2 (FiltCP.allpassPhaser 10))
                 (FiltCP.allpassPhaser 10)) :
         ("moog-lowpass",
              renderFilter 20
                 id
                 (Filt.moogLowpass 10)
                 (FiltCP.moogLowpass 10)) :
         ("universal-lowpass",
              renderFilter 20
                 Filt.lowpassFromUniversal
                 Filt.universal
                 FiltCP.universal) :
         ("butterworth-lowpass",
              renderFilter 0.5
                 id
                 (Filt.butterworthLowpass 10)
                 (FiltCP.butterworthLowpass 10)) :
         ("butterworth-highpass",
              renderFilter 0.5
                 id
                 (Filt.butterworthHighpass 10)
                 (FiltCP.butterworthHighpass 10)) :
         ("chebyshev-a-lowpass",
              renderFilter 0.5
                 id
                 (Filt.chebyshevALowpass 10)
                 (FiltCP.chebyshevALowpass 10)) :
         ("chebyshev-a-highpass",
              renderFilter 0.5
                 id
                 (Filt.chebyshevAHighpass 10)
                 (FiltCP.chebyshevAHighpass 10)) :
         ("chebyshev-b-lowpass",
              renderFilter 0.5
                 id
                 (Filt.chebyshevBLowpass 10)
                 (FiltCP.chebyshevBLowpass 10)) :
         ("chebyshev-b-highpass",
              renderFilter 0.5
                 id
                 (Filt.chebyshevBHighpass 10)
                 (FiltCP.chebyshevBHighpass 10)) :
         []

      mapM_
         (\(name, sound) ->
             renderToAIFF
             File.renderTimeVoltageMonoDoubleToInt16
             name (fromSound sound)) $

         {-
         Moog, Allpass, Universal.lowPass are redundant here,
         but we leave them for demonstration purposes.
         -}
         ("moog-saw-direct",
                         Sound (Cut.take (DN.time 10) $: moogSawDirect)) :
         ("moog-saw-causal",
                         Sound (Cut.take (DN.time 10) $: moogSawCausal)) :

         ("allpass-phaser-direct",
                         Sound (Cut.take (DN.time 10) $: allpassPhaserDirect)) :
         ("allpass-phaser-causal",
                         Sound (Cut.take (DN.time 10) $: allpassPhaserCausal)) :

         ("universal-lowpass",
                         Sound (Cut.take (DN.time 10) $: universalLowpassDirect)) :
         ("universal-lowpass-sync",
                         Sound (Cut.take (DN.time 10) $: universalLowpassSync)) :
         ("universal-lowpass-async-linear",
                         Sound (Cut.take (DN.time 10) $: universalLowpassAsyncLinear)) :
         ("universal-lowpass-async-constant",
                         Sound (Cut.take (DN.time 10) $: universalLowpassAsyncConstant)) :

         ("sine-low",    Sound (Cut.take (DN.time 1) $: sineLow)) :
         ("sine-high",   Sound (Cut.take (DN.time 1) $: sineHigh)) :
         ("sine-mix",    Sound (Cut.take (DN.time 1) $: sineMix)) :
         ("exponential", Sound (Cut.take (DN.time 1) $: DN.voltage 1 &*^ exponential)) :
         ("ping",        Sound (Cut.take (DN.time 1) $: ping)) :
         ("ping-saw",    Sound (Cut.take (DN.time 1) $: pingSaw)) :

--         ("saw",         Sound (Cut.take (DN.time 2) $: saw)) :
         ("saw-vibrato", Sound (Cut.take (DN.time 2) $: sawVibrato)) :
         ("saw-chorus",  Sound (Cut.take (DN.time 2) $: sawChorus)) :

         ("wasp",        Sound (Cut.take (DN.time  5) $: wasp (DN.frequency 110))) :
         ("trapezoid",   Sound (Cut.take (DN.time  5) $: trapezoid)) :
         ("damped-echo", Sound (Cut.take (DN.time  4) $: dampedEcho)) :
         ("chirp",       Sound (amplitudeModulationChirp)) :
         ("airplane",        Sound airplane) :
         {- This becomes considerably faster, if other effects are not rendered.
            This is obviously an optimizer bug. -}
         ("airplane-fade",   Sound airplaneFade) :

         ("noise-lowpass1",  Sound noiseLowpass) :
         ("noise-highpass1", Sound noiseHighpass) :
         []
{-
      flip mapM_ waveformsBandlimited $
         \(fileName, tone) ->
            renderToAIFF
            File.renderTimeVoltageMonoDoubleToInt16
            fileName
            (fromSound tone)
-}
      flip mapM_ (harmonicExamples ++ harmonicMorph ++ waveforms) $
         \(fileName, tone) ->
            renderToAIFF
            File.renderTimeVoltageMonoDoubleToInt16
            fileName
            (Cut.take (DN.time 1) $: fromSound tone)


{-
import installed synthesizer package

ghc-core -f html -- -o dist/build/demonstration/demonstration -Wall -O2 -fexcess-precision -fvia-C -optc-O2 -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs >dist/build/demonstration/demonstration.html

ghc -o dist/build/demonstration/demonstration -Wall -O2 -fexcess-precision -fvia-C -optc-O2 -ddump-simpl-stats -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs

ghc -o dist/build/demonstration/demonstration -O -Wall -fexcess-precision -ddump-simpl-stats -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs

ghc -o dist/build/demonstration/demonstration -O -Wall -fexcess-precision -ddump-simpl -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs >dist/build/Demonstration.log


with assembly output

ghc -o dist/build/fusiontest/fusiontest -O -Wall -fexcess-precision -ddump-simpl-stats -ddump-asm -package synthesizer speedtest/DemonstrationInlineMono.hs >dist/build/Demonstration.asm


with make and no explicit package specification:

ghc -Idist/build -o dist/build/demonstration/demonstration --make -Wall -O -fexcess-precision -ddump-simpl-stats -i -idist/build/autogen -isrc -odir dist/build/demonstration/demonstration-tmp -hidir dist/build/demonstration/demonstration-tmp src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs


with make and explicit package specification:

ghc --make -Idist/build -o dist/build/demonstration/demonstration -Wall -O -fexcess-precision -ddump-simpl-stats -ddump-simpl-iterations -i -idist/build/autogen -isrc -idist/build/demonstration/demonstration-tmp -odir dist/build/demonstration/demonstration-tmp -hidir dist/build/demonstration/demonstration-tmp -package base-1.0 -package mtl-1.0 -package non-negative-0.0.2 -package numeric-prelude-0.0.3 -package event-list-0.0.7 -package bytestring-0.9.0.5 -package binary-0.4.1 -package storablevector-0.1  src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs >src/Synthesizer/Dimensional/RateAmplitude/Demonstration.log

without make and with detailed simplifier report:

ghc -Idist/build -o dist/build/demonstration/demonstration -Wall -O -fexcess-precision -ddump-simpl-stats -ddump-simpl-iterations -i -idist/build/autogen -isrc -idist/build/demonstration/demonstration-tmp -odir dist/build/demonstration/demonstration-tmp -hidir dist/build/demonstration/demonstration-tmp -package base-1.0 -package mtl-1.0 -package non-negative-0.0.2 -package numeric-prelude-0.0.3 -package event-list-0.0.7 -package HTam-0.0 -package numeric-quest-0.1 -package bytestring-0.9.0.5 -package binary-0.4.1 -package storablevector-0.1 dist/build/HSsynthesizer*.o src/Synthesizer/Dimensional/RateAmplitude/Demonstration.hs  >src/Synthesizer/Dimensional/RateAmplitude/Demonstration.log
-}
