{-
A set of example instruments to be used in MIDI rendering.

Shall we make the sample rate a parameter
or shall we leave these examples at a low level?
Sample-rate-aware instruments can be found in
"Synthesizer.MIDI.Dimensional.Example.Instrument"
-}
module Synthesizer.MIDI.Example.Instrument where

import Synthesizer.MIDI.Storable (
   Instrument, chunkSizesFromLazyTime, )

import Synthesizer.MIDI.EventList (LazyTime, )

import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.CausalIO.Gate as Gate
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.Basic.Wave          as Wave
import qualified Synthesizer.Frame.Stereo        as Stereo

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Causal.Oscillator as OsciC
import qualified Synthesizer.Causal.Interpolation as Interpolation
import qualified Synthesizer.Causal.Filter.Recursive.Integration as IntegC
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltNRC
import qualified Synthesizer.Interpolation.Module as Ip
import Control.Arrow ((<<<), (^<<), (<<^), (***), )

import qualified Synthesizer.Storable.Filter.NonRecursive as FiltNRSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy         as SVL
import qualified Data.StorableVector              as SV

import qualified Synthesizer.Generic.Wave      as WaveG
import qualified Synthesizer.State.Signal      as SigS
import qualified Synthesizer.State.Control     as CtrlS
import qualified Synthesizer.State.Noise       as NoiseS
import qualified Synthesizer.State.Oscillator  as OsciS
import qualified Synthesizer.State.Displacement as DispS
import qualified Synthesizer.State.Filter.NonRecursive as FiltNRS
import qualified Synthesizer.Plain.Filter.Recursive    as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
-- import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
-- import qualified Synthesizer.Basic.Phase       as Phase

import qualified Sound.Sox.Read          as SoxRead
import qualified Sound.Sox.Option.Format as SoxOption

import Control.Monad.Trans.State (get, modify, )
import Control.Monad (mzero, )
import Control.Category ((.), )

import qualified Algebra.Ring as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.))
import Prelude ()



type Real = Float

sampleRate :: Ring.C a => a
sampleRate = fromInteger 44100

chunkSize :: SVL.ChunkSize
chunkSize = SVL.chunkSize 512


{-# INLINE amplitudeFromVelocity #-}
amplitudeFromVelocity :: Real -> Real
amplitudeFromVelocity vel = 4**vel

{-# INLINE ping #-}
ping :: Real -> Real -> SigSt.T Real
ping vel freq =
   SigS.toStorableSignal chunkSize $
   FiltNRS.envelope (CtrlS.exponential2 (0.2*sampleRate) (amplitudeFromVelocity vel)) $
   OsciS.static Wave.saw zero (freq/sampleRate)

pingDur :: Instrument Real Real
pingDur vel freq dur =
   SigStV.take (chunkSizesFromLazyTime dur) $
   ping vel freq

pingCausal :: MIO.Instrument Real (SV.Vector Real)
pingCausal vel freq =
   (PIO.fromCausal $
    Causal.applyStorableChunk $
    Causal.feed $
    FiltNRS.envelope
       (CtrlS.exponential2 (0.2*sampleRate) (amplitudeFromVelocity vel)) $
    OsciS.static Wave.saw zero (freq/sampleRate))
   <<<
   Gate.toStorableVector

pingReleaseEnvelope :: Real -> LazyTime -> SigSt.T Real
pingReleaseEnvelope vel dur =
   SigSt.switchR SigSt.empty
      (\body x ->
          SigSt.append body $
          SigS.toStorableSignal chunkSize $
          SigS.take (round (0.3*sampleRate :: Real)) $
          CtrlS.exponential2 (0.1*sampleRate) x) $
   SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
   CtrlS.exponential2 (0.4*sampleRate) (amplitudeFromVelocity vel)

pingRelease :: Instrument Real Real
pingRelease vel freq dur =
   SigS.zipWithStorable (*)
      (OsciS.static Wave.saw zero (freq/sampleRate))
      (pingReleaseEnvelope vel dur)

pingStereoRelease :: Instrument Real (Stereo.T Real)
pingStereoRelease vel freq dur =
--   SigS.zipWithStorable (\y c -> fmap (c*) y)
   SigS.zipWithStorable (flip (*>))
      (SigS.zipWith Stereo.cons
         (OsciS.static Wave.saw zero (freq*0.999/sampleRate))
         (OsciS.static Wave.saw zero (freq*1.001/sampleRate)))
      (pingReleaseEnvelope vel dur)

pingReleaseEnvelopeCausal :: Real -> PIO.T MIO.GateChunk (SV.Vector Real)
pingReleaseEnvelopeCausal vel =
   PIO.continue
      ((PIO.fromCausal $
        Causal.applyStorableChunk $ Causal.feed $
        CtrlS.exponential2 (0.4*sampleRate) (amplitudeFromVelocity vel))
       <<<
       Gate.toStorableVector
       {-
       <<<
       arr (\x -> trace (show x) x) -})
      (\y -> -- trace ("continue with " ++ show y) $
         (PIO.fromCausal $
          Causal.applyStorableChunk $ Causal.feed $
          SigS.take (round (1*sampleRate :: Real)) $
          CtrlS.exponential2 (0.1*sampleRate) y)
         <<<
         Gate.allToStorableVector)

pingReleaseCausal :: MIO.Instrument Real (SV.Vector Real)
pingReleaseCausal vel freq =
   (PIO.fromCausal $
    Causal.applyStorableChunk $
    FiltNRC.envelope <<<
    Causal.feedFst (OsciS.static Wave.saw zero (freq/sampleRate)))
   <<<
   pingReleaseEnvelopeCausal vel

tine :: Instrument Real Real
tine vel freq dur =
   SigS.zipWithStorable (*)
      (OsciS.phaseMod Wave.sine (freq/sampleRate)
         (FiltNRS.envelope
            (CtrlS.exponential (1*sampleRate) (vel+1))
            (OsciS.static Wave.sine zero (2*freq/sampleRate))))
      (pingReleaseEnvelope 0 dur)

tineStereo :: Instrument Real (Stereo.T Real)
tineStereo vel freq dur =
   let ctrl f =
          FiltNRS.envelope
             (CtrlS.exponential (1*sampleRate) (vel+1))
             (OsciS.static Wave.sine zero (2*f/sampleRate))
   in  SigS.zipWithStorable (flip (*>))
          (SigS.zipWith Stereo.cons
             (OsciS.phaseMod Wave.sine (freq*0.995/sampleRate) (ctrl freq))
             (OsciS.phaseMod Wave.sine (freq*1.005/sampleRate) (ctrl freq)))
          (pingReleaseEnvelope 0 dur)


softStringReleaseEnvelope ::
   Real -> LazyTime -> SigSt.T Real
softStringReleaseEnvelope vel dur =
   let attackTime = sampleRate
       amp = amplitudeFromVelocity vel
       cnst = CtrlS.constant amp
       {-
       release <- take attackTime beginning
       would yield a space leak, thus we first split 'beginning'
       and then concatenate it again
       -}
       {-
       We can not easily generate attack and sustain separately,
       because we want to use the chunk structure implied by 'dur'.
       -}
       (attack, sustain) =
          SigSt.splitAt attackTime $
          SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
          flip SigS.append cnst $
          SigS.map ((amp*) . sin) $
          CtrlS.line attackTime (0, pi/2)
       release = SigSt.reverse attack
   in  attack `SigSt.append` sustain `SigSt.append` release

-- it's better to avoid inlining here
softString :: Instrument Real (Stereo.T Real)
softString vel freq dur =
   let f = freq/sampleRate
       {-# INLINE osci #-}
       osci d =
          OsciS.static Wave.saw zero (d * f)
   in  flip (SigS.zipWithStorable (flip (*>)))
          (softStringReleaseEnvelope vel dur)
          (SigS.map ((0.3::Real)*>) $
           SigS.zipWith Stereo.cons
             (DispS.mix
                (osci 1.005)
                (osci 0.998))
             (DispS.mix
                (osci 1.002)
                (osci 0.995)))


softStringReleaseEnvelopeCausal ::
   Real -> LazyTime -> SigSt.T Real
softStringReleaseEnvelopeCausal vel dur =
   Causal.apply
      (softStringReleaseEnvelopeCausalProcess vel)
      (SigSt.append
         (SigStV.replicate (chunkSizesFromLazyTime dur) True)
         (SigSt.repeat chunkSize False))


{-# INLINE softStringReleaseEnvelopeCausalProcess #-}
softStringReleaseEnvelopeCausalProcess ::
   Real -> Causal.T Bool Real
softStringReleaseEnvelopeCausalProcess vel =
   let vol = amplitudeFromVelocity vel
       attackTime = sampleRate
       {-# INLINE sine #-}
       sine x = sin (x*pi/(2*attackTime))
   in  Causal.fromStateMaybe
          (\b ->
             get >>= \n ->
             if b
               then
                 if n==attackTime
                   then return vol
                   else
                     modify (1+) >>
                     return (vol * sine n)
               else
                 if n==0
                   then mzero
                   else
                     modify (subtract 1) >>
                     return (vol * sine n))
          zero

{-# INLINE softStringCausalProcess #-}
softStringCausalProcess :: Real -> Causal.T Real (Stereo.T Real)
softStringCausalProcess freq =
   let f = freq/sampleRate
       {-# INLINE osci #-}
       osci d =
          OsciS.static Wave.saw zero (d * f)
   in  Causal.applySnd
          (Causal.map (uncurry (*>)))
          (SigS.map ((0.3::Real)*>) $
           SigS.zipWith Stereo.cons
             (DispS.mix
                (osci 1.005)
                (osci 0.998))
             (DispS.mix
                (osci 1.002)
                (osci 0.995)))

softStringCausal :: Instrument Real (Stereo.T Real)
softStringCausal vel freq dur =
   Causal.apply
      (softStringCausalProcess freq <<<
       softStringReleaseEnvelopeCausalProcess vel)
      (SigSt.append
         (SigStV.replicate (chunkSizesFromLazyTime dur) True)
         (SigSt.repeat chunkSize False))


stringStereoFM :: SigSt.T Real -> Instrument Real (Stereo.T Real)
stringStereoFM fmSt vel freq dur =
   let fm = SigS.fromStorableSignal fmSt
   in  SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
       FiltNRS.amplifyVector (amplitudeFromVelocity vel) $
       SigS.zipWith Stereo.cons
          (OsciS.freqMod Wave.saw zero $
           FiltNRS.amplify (freq*0.999/sampleRate) fm)
          (OsciS.freqMod Wave.saw zero $
           FiltNRS.amplify (freq*1.001/sampleRate) fm)

stringStereoDetuneFM ::
   SigSt.T Real -> SigSt.T Real -> Instrument Real (Stereo.T Real)
stringStereoDetuneFM detuneSt fmSt vel freq dur =
   let fm = SigS.fromStorableSignal fmSt
       detune = SigS.fromStorableSignal detuneSt
       {-# INLINE osci #-}
       osci =
          OsciS.freqMod Wave.saw zero .
          FiltNRS.amplify (freq/sampleRate) .
          FiltNRS.envelope fm
   in  SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
       FiltNRS.amplifyVector (amplitudeFromVelocity vel) $
       SigS.zipWith Stereo.cons
          (osci $ SigS.map (1-) detune)
          (osci $ SigS.map (1+) detune)

{-# INLINE sampledSoundGenerator #-}
sampledSoundGenerator :: (Real, SigSt.T Real) -> Real -> SigS.T Real
sampledSoundGenerator (period, sample) freq =
   Causal.apply
      (Interpolation.relativeZeroPad zero Ip.linear zero
          (SigS.fromStorableSignal sample)) $
   SigS.repeat (freq/sampleRate*period)

sampledSound :: (Real, SigSt.T Real) -> Instrument Real Real
sampledSound sound vel freq dur =
   SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
   SigS.map (amplitudeFromVelocity vel *) $
   sampledSoundGenerator sound freq

sampledSoundDetuneStereo ::
   Real -> (Real, SigSt.T Real) -> Instrument Real (Stereo.T Real)
sampledSoundDetuneStereo detune sound vel freq dur =
   SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
   SigS.map (amplitudeFromVelocity vel *>) $
   SigS.zipWith Stereo.cons
      (sampledSoundGenerator sound (freq*(1-detune)))
      (sampledSoundGenerator sound (freq*(1+detune)))

sampleReleaseEnvelope :: Real -> Real -> LazyTime -> SigSt.T Real
sampleReleaseEnvelope halfLife vel dur =
   let amp = amplitudeFromVelocity vel
   in  SigSt.append
          (SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
           CtrlS.constant amp)
          (SigS.toStorableSignal chunkSize $
           SigS.take (round (5*halfLife*sampleRate :: Real)) $
           CtrlS.exponential2 (halfLife*sampleRate) amp)

sampledSoundDetuneStereoRelease ::
   Real -> Real -> (Real, SigSt.T Real) -> Instrument Real (Stereo.T Real)
sampledSoundDetuneStereoRelease release detune sound vel freq dur =
   flip (SigS.zipWithStorable (flip (*>)))
      (sampleReleaseEnvelope release vel dur) $
   SigS.zipWith Stereo.cons
      (sampledSoundGenerator sound (freq*(1-detune)))
      (sampledSoundGenerator sound (freq*(1+detune)))


readPianoSample :: IO (Real, SigSt.T Real)
readPianoSample =
   fmap ((,) 96) $
   SoxRead.withHandle1 (SVL.hGetContentsSync chunkSize) =<<
   SoxRead.open SoxOption.none "a-piano3"

readStringSample :: IO (Real, SigSt.T Real)
readStringSample =
   fmap ((,) 64) $
   SoxRead.withHandle1 (SVL.hGetContentsSync chunkSize) =<<
   SoxRead.open SoxOption.none "strings7.s8"


{- |
Resample a sampled sound with a smooth loop
using our time manipulation algorithm.
Time is first controlled linearly,
then switches to a sine or triangular control.
Loop start must be large enough in order provide enough spare data
for interpolation at the beginning
and loop start plus length must preserve according space at the end.
One period is enough space for linear interpolation.
The infinite sound we generate is not just a cycle,
that uses bounded space.
Instead we need to compute all the time.
In order to avoid duplicate interpolation,
we have merged resampling and time looping.
-}
{-# INLINE sampledSoundTimeLoop #-}
sampledSoundTimeLoop ::
   (Real -> Real -> Real -> Real -> SigS.T Real) ->
   (Real, SigSt.T Real) -> Real -> Real -> Instrument Real Real
sampledSoundTimeLoop loopTimeMod
     (period, sample) loopLen loopStart vel freq dur =
   let wave = WaveG.sampledTone Ip.linear Ip.linear period sample
   in  SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
       (((0.2 * amplitudeFromVelocity vel) *) ^<<
        OsciC.shapeMod wave zero (freq/sampleRate))
       `Causal.apply`
          loopTimeMod period (loopLen/2) (loopStart + loopLen/2) freq

{-
Graphics.Gnuplot.Simple.plotList [] (SigS.toList $ SigS.take 20000 $ loopTimeMod 64 1000 2000 440)
-}
loopTimeModSine :: Real -> Real -> Real -> Real -> SigS.T Real
loopTimeModSine period loopDepth loopCenter freq =
   let rate = freq*period/sampleRate
   in  SigS.append
          (SigS.takeWhile (loopCenter>=) $
           SigS.iterate (rate+) zero)
          (SigS.map (\t -> loopCenter + loopDepth * sin t) $
           SigS.iterate ((rate/loopDepth)+) zero)

loopTimeModZigZag :: Real -> Real -> Real -> Real -> SigS.T Real
loopTimeModZigZag period loopDepth loopCenter freq =
   let rate = freq*period/sampleRate
   in  SigS.append
          (SigS.takeWhile (loopCenter>=) $
           SigS.iterate (rate+) zero)
          (SigS.map (\t -> loopCenter + loopDepth * t) $
           OsciS.static Wave.triangle zero (rate/(4*loopDepth)))



timeModulatedSample :: (Real, SigSt.T Real) ->
   SigSt.T Real -> SigSt.T Real -> SigSt.T Real -> Instrument Real Real
timeModulatedSample (period, sample) offsetMod speedMod freqMod vel freq dur =
   let wave = WaveG.sampledTone Ip.linear Ip.linear period sample
   in  SigStV.take (chunkSizesFromLazyTime dur) $
{-
       (((0.2 * amplitudeFromVelocity vel) *) ^<<
        OsciC.freqMod Wave.saw zero <<<
        Causal.map ((freq/sampleRate) *))
       `Causal.apply` freqMod
-}
       (((0.2 * amplitudeFromVelocity vel) *) ^<<
        OsciC.shapeFreqMod wave zero <<<
        (uncurry (+) ^<< Causal.feedFst offsetMod <<< IntegC.run) ***
         Causal.map ((freq/sampleRate) *))
       `Causal.applyFst` speedMod
       `Causal.apply` freqMod


colourNoise ::
   SigSt.T Real -> SigSt.T Real ->
   Instrument Real Real
colourNoise resonanceMod freqMod vel freq dur =
   SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
   ((((sqrt sampleRate/2000 * amplitudeFromVelocity vel) *) . UniFilter.lowpass) ^<<
    UniFilter.causal)
   `Causal.applyFst`
      SigS.zipWith
         (\r f -> UniFilter.parameter $ FiltR.Pole r (f*freq/sampleRate))
         (SigS.fromStorableSignal resonanceMod)
         (SigS.fromStorableSignal freqMod)
   `Causal.apply` NoiseS.white


toneFromNoise ::
   SigSt.T Real -> SigSt.T Real ->
   Instrument Real Real
toneFromNoise speedMod freqMod vel freq dur =
   SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
   (((0.1 * amplitudeFromVelocity vel) *) ^<<
    OsciC.shapeFreqModFromSampledTone
       Ip.linear Ip.linear
       100 (SigS.toStorableSignal chunkSize NoiseS.white)
       zero zero <<<
    Causal.second (Causal.map ((freq/sampleRate)*)))
   `Causal.applyFst`
      SigS.fromStorableSignal speedMod
   `Causal.apply`
      SigS.fromStorableSignal freqMod

{-
I like to control the filter parameters
before phase and time modulation.
Unfortunately this means,
that we have to translate those control signals back
using the speed profile, which is tricky.
We need an inverse frequency modulation, that is:

freqMod ctrl (invFreqMod ctrl signal) = signal

The problem is, that the chunk boundaries will not match.
invFreqMod must be a StorableSignal function and it is not causal
in any of its inputs.
-}
toneFromFilteredNoise ::
   SigSt.T Real -> SigSt.T Real ->
   SigSt.T Real -> SigSt.T Real ->
   Instrument Real Real
toneFromFilteredNoise resonanceMod cutoffMod speedMod freqMod vel freq dur =
   let period = 100
       filtNoise =
          (((amplitudeFromVelocity vel *) . UniFilter.lowpass) ^<<
           UniFilter.causal <<< Causal.feedSnd NoiseS.white
           <<^ (\(r,f) -> UniFilter.parameter $
                  FiltR.Pole r (f/period)))
          `Causal.applyFst`
             FiltNRSt.inverseFrequencyModulationFloor chunkSize speedMod resonanceMod
          `Causal.apply`
             FiltNRSt.inverseFrequencyModulationFloor chunkSize speedMod cutoffMod
   in  SigStV.take (chunkSizesFromLazyTime dur) $
       (((0.1 * amplitudeFromVelocity vel) *) ^<<
        OsciC.shapeFreqModFromSampledTone
           Ip.linear Ip.linear
           period filtNoise
           zero zero <<<
        Causal.second (Causal.map ((freq/sampleRate)*)))
       `Causal.applyFst` speedMod
       `Causal.apply`    freqMod
