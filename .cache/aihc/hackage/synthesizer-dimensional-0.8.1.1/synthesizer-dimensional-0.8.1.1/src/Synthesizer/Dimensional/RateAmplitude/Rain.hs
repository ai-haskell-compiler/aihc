{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where
-- module Synthesizer.Dimensional.RateAmplitude.Rain where

-- import qualified Synthesizer.Dimensional.RateAmplitude.Instrument as Instr

import qualified Synthesizer.Dimensional.Rate.Oscillator as Osci
import qualified Synthesizer.Dimensional.Rate.Filter     as Filt
import qualified Synthesizer.Dimensional.RateAmplitude.Displacement as Disp
-- import qualified Synthesizer.Dimensional.RateAmplitude.Noise      as Noise
import qualified Synthesizer.Dimensional.RateAmplitude.Cut        as Cut
import qualified Synthesizer.Dimensional.Amplitude.Filter         as FiltA
import qualified Synthesizer.Dimensional.Amplitude.Cut            as CutA

import qualified Synthesizer.Dimensional.RateAmplitude.Piece      as Piece
import qualified Synthesizer.Dimensional.RateAmplitude.Control    as Ctrl
import qualified Synthesizer.Dimensional.Rate.Control             as CtrlR
import qualified Synthesizer.Dimensional.Rate.Cut                 as CutR

import qualified Synthesizer.Dimensional.Wave.Controlled as WaveCtrl
import qualified Synthesizer.Dimensional.Wave as WaveD
import qualified Synthesizer.Dimensional.Arrow as ArrowD

import Synthesizer.Dimensional.Wave ((&*~), )
import Control.Arrow ((<<<), first, )

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal as SigA

import qualified Synthesizer.Dimensional.RateAmplitude.File as File
-- import qualified Synthesizer.Dimensional.RateAmplitude.Play as Play

import Synthesizer.Dimensional.Signal ((&*^), (&*>^), )
import Synthesizer.Dimensional.Process (($:), ($::), ($^), (.:), (.^), )
import Synthesizer.Dimensional.Amplitude.Displacement (mapExponential, )
import Synthesizer.Dimensional.RateAmplitude.Piece
          ((|#), (#|), (-|#), (#|-), )

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Synthesizer.Plain.Control as CtrlL
import qualified Synthesizer.Plain.Displacement as DispL
import qualified Synthesizer.Plain.Noise as NoiseL
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltL
import qualified Synthesizer.Plain.Oscillator as OsciL
-- import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Basic.Phase as Phase

import Synthesizer.Utility (balanceLevel, )

import qualified Synthesizer.Storable.Signal as SigSt

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import Number.DimensionTerm ((*&))

import qualified Number.NonNegative     as NonNeg

import qualified Algebra.Ring           as Ring

import qualified Data.EventList.Relative.TimeBody as EventList

-- import Foreign.Storable (Storable, )

import Control.Applicative (liftA2, )
import Data.Maybe.HT (toMaybe, )
import Data.List (genericLength, )

import System.Random (randoms, randomRs, mkStdGen, )

import NumericPrelude.Base
import NumericPrelude.Numeric


type PitchClass = Int

type Pitch = (PitchClass, Int)

c, d, e, f, g, a, h :: PitchClass
c =  0
d =  2
e =  4
f =  5
g =  7
a =  9
h = 11

chords, _chords0, chords1, _chords2 :: [([PitchClass],Int)]
chords = chords1

_chords0 =
   ([c,e,g], 4) :
   ([c,e,a], 1) :
   ([d,g,h], 1) :
   ([c,f,a], 1) :
   ([c,e,g], 2) :
   []

chords1 =
   ([c,e,g], 2) :
   ([c,e,a], 1) :
   ([d,g,h], 1) :
   ([c,f,a], 1) :
   ([c,e,g], 1) :
   []

_chords2 =
   ([c,e,g], 1) :
   ([c,e,a], 1) :
   ([c,e,g], 1) :
   []


chordTicks :: Int
chordTicks =
   150
   -- 200

{-# INLINE assemblePitch #-}
assemblePitch :: Pitch -> Double
assemblePitch (pc, oct) =
   fromIntegral pc / 12 + fromIntegral oct


type Signal s amp a = SigA.T (Rate.Phantom s) amp (SigSt.T a)
type Flat = Amp.Flat Double
type Volt = Amp.Dimensional Dim.Voltage Double

{-
delay ::
   (SigG2.Transform sig y (Stereo.T y), SigG.Write sig y,
    Additive.C y, Amp.Primitive amp,
    RealField.C t, Dim.C u) =>
   DN.T u t ->
   Proc.T s u t
      (SigA.T (Rate.Phantom s) amp (sig y) ->
       SigA.T (Rate.Phantom s) amp (sig (Stereo.T y)))
-}
delay ::
   DN.Time Double ->
   Proc.T s Dim.Time Double
      (Signal s Flat Double -> Signal s Flat (Stereo.T Double))
delay time =
   let (appDelay, merge) =
          if time>=zero
            then (Filt.delay time, flip CutA.mergeStereoPrimitive)
            else (Filt.delay (negate time), CutA.mergeStereoPrimitive)
   in  flip fmap appDelay
          (\del x -> merge x (del x))

{-# INLINE bell #-}
bell ::
   DN.Time Double ->
   DN.Frequency Double ->
   Proc.T s Dim.Time Double (Signal s Flat (Stereo.T Double))
bell del freq =
   delay del .:
   SigA.store timeUnit .:
   CutR.take (DN.time 1) .:
   (Filt.envelope $: CtrlR.exponential2 (DN.time 0.2)) $:
   Osci.static (WaveD.flat Wave.sine) zero freq


{-# INLINE deinterleave #-}
deinterleave :: [a] -> [(a,a)]
deinterleave (x0:x1:xs) =
   (x0,x1) : deinterleave xs
deinterleave [] = []
deinterleave _ =
   error "deinterleave: input list must have even length"

stringAttackTicks :: Int
stringAttackTicks = 50

stringAttack :: DN.Time Double
stringAttack =
   fromIntegral stringAttackTicks *& timeUnit

stringEnvelope ::
   DN.Time Double ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Scalar Double Double)
stringEnvelope duration =
   Piece.runState $
      DN.scalar 0.01 |#
         (stringAttack,
          Piece.halfSine Piece.FlatRight) #|-
      DN.scalar 1 -|#
         (duration - stringAttack, Piece.step) #|-
      DN.scalar 1 -|#
         (stringAttack,
          Piece.halfSine Piece.FlatLeft) #|
      DN.scalar 0.01

_stringDistortion ::
   DN.Time Double ->
   DN.Voltage Double ->
   DN.Frequency Double ->
   Phase.T Double ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
_stringDistortion duration volume freq phase =
   Disp.distort sin
    $: (volume &*^ stringEnvelope duration)
    $: Osci.static (volume &*~ Wave.saw) phase freq

{-# INLINE stringMorph #-}
{-# INLINE _stringMorph2 #-}
{-# INLINE _stringMorph3 #-}
{-# INLINE _stringMorph4 #-}
stringMorph, _stringMorph2, _stringMorph3, _stringMorph4 ::
   DN.Time Double ->
   DN.Voltage Double ->
   DN.Frequency Double ->
   Phase.T Double ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
stringMorph duration volume freq phase =
   Osci.shapeMod
      (makeWave volume
          (\r -> Wave.distort (sin . ((pi/2*r)*)) Wave.saw))
      phase freq
    $: Ctrl.line (stringAttack + duration)
          (DN.scalar 1, DN.scalar 7)

_stringMorph2 duration volume freq phase =
   Osci.shapeMod
      (makeWave volume Wave.truncCosine)
      phase freq
    $: Ctrl.line (stringAttack + duration)
          (DN.scalar 1, DN.scalar 7)

_stringMorph3 duration volume freq phase =
   Osci.shapeMod
      (makeWave volume (Wave.powerNormed2 . (^2)))
      phase freq
    $: Ctrl.line (stringAttack + duration)
          (DN.scalar 0.1, DN.scalar 2)

_stringMorph4 duration volume freq phase =
   Osci.shapeMod
      (makeWave volume (Wave.trapezoidSkew . (^2)))
      phase freq
    $: Ctrl.line (stringAttack + duration)
          (DN.scalar 0, DN.scalar 1)

makeWave :: (Ring.C y, Dim.C u, Flat.C c flat) =>
   DN.T u y ->
   (c -> Wave.T t y) ->
   WaveCtrl.T
      (Sample.T flat c) t
      (Sample.Dimensional u y y)
makeWave volume wave =
   WaveCtrl.amplified volume wave <<<
   first ArrowD.canonicalizeFlat

{-# INLINE strings #-}
strings ::
   DN.Time Double ->
   DN.Frequency Double ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
strings duration mainFreq =
   let n = 4
       volume = DN.voltage 0.05
--       volume = recip (sqrt (fromIntegral n)) *& DN.voltage 0.3
       {-# INLINE freqs #-}
       freqs =
          map (flip DN.scale mainFreq) $
          balanceLevel 1 $ take (2*n) $
          randomRs (-0.02, 0.02) $
          mkStdGen 912
       phases =
          randoms $ mkStdGen 54
       tones =
          zipWith (stringMorph duration volume) freqs phases
   in  Filt.firstOrderLowpass
          $: (mapExponential 1000 (DN.frequency 5) $^ stringEnvelope duration)
          $: (Disp.mixMulti $::
               (map (uncurry (liftA2 CutA.mergeStereo)) $
                deinterleave tones))

{-
{-# INLINE strings #-}
strings ::
   DN.Frequency Double ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
strings freq =
   let n = 5
       volume = recip (sqrt (fromIntegral n)) *& DN.voltage 0.5
       range = 0.03 *& freq
       {-# INLINE freqs #-}
       freqs =
          balanceLevel freq $ take (2*n) $
          randomRs (-range, range) $
          mkStdGen 912
       phases =
          randoms $ mkStdGen 54
       tones =
          zipWith
             (\freq phase ->
                 Osci.static (WaveD.flat Wave.saw) phase freq)
             freqs phases
   in  volume &*>^
       (Disp.mixMulti $::
        (liftA2 (uncurry CutA.mergeStereoPrimitive) $
         deinterleave tones))
-}

{-# INLINE chordSnds #-}
chordSnds ::
   Proc.T s Dim.Time Double
      (EventList.T NonNeg.Double (Signal s Volt (Stereo.T Double)))
chordSnds =
   EventList.traverseBody
      (\(tones,dur) ->
          (SigA.store timeUnit .:
           Disp.mixMulti) $:
          mapM
             (strings (fromIntegral (dur*chordTicks) *& timeUnit) .
              (*& DN.frequency 440) . (2**) .
              assemblePitch . flip (,) 0)
             tones) $
   EventList.fromPairList $
   zip (map fromIntegral $ zero : map ((chordTicks*) . snd) chords) chords

partTicks :: NonNeg.Double
partTicks =
   fromIntegral $ chordTicks * sum (map snd chords)

chordStartTicks :: NonNeg.Double
chordStartTicks =
   partTicks - fromIntegral stringAttackTicks / 2


{-# INLINE timeUnit #-}
timeUnit :: DN.Time Double
timeUnit = DN.time 0.05


noteFromFraction :: [PitchClass] -> Double -> Pitch
noteFromFraction tones x =
   let (oct,p) = splitFraction x
   in  (tones!!floor(p*genericLength tones), oct)

drops ::
   EventList.T NonNeg.Double
      (DN.Voltage Double,
       DN.Time Double,
       DN.Frequency Double)
drops =
   -- Attention: This requires storage of the list, but it should not consume too much memory
   (\es -> EventList.append es es) $
   EventList.fromPairList $
   map ((,) 1) $
   zip3
      (randomRs (DN.voltage 0, DN.voltage 0.3) (mkStdGen 58))
      (randomRs (DN.time (-0.01), DN.time 0.01) (mkStdGen 85)) $
   map ((*& DN.frequency 440) . (2**) . (2+) .
        assemblePitch) $
   zipWith noteFromFraction
      (concatMap (uncurry $ flip $ replicate . (chordTicks*)) chords) $
   DispL.mix (OsciL.static Wave.sine 0 0.003) $
   FiltL.amplify 0.5 $
   NoiseL.whiteQuadraticBSplineGen (mkStdGen 39847)

-- these lists must be inlined, otherwise they will blow up the heap
{-# INLINE evolvingDrops #-}
evolvingDrops ::
   EventList.T NonNeg.Double
      (DN.Voltage Double,
       DN.Time Double,
       DN.Frequency Double)
evolvingDrops =
   EventList.catMaybes $
   EventList.zipWithBody toMaybe
      (zipWith (<)
          (CtrlL.exponential2 1000 (1::Double))
          (randomRs (0,1) (mkStdGen 42))) $
   drops

{-# INLINE evolvingDropSnds #-}
evolvingDropSnds ::
   Proc.T s Dim.Time Double
      (EventList.T NonNeg.Double (Signal s Volt (Stereo.T Double)))
evolvingDropSnds =
   EventList.traverseBody
      (\(vol,time,freq) ->
          vol &*>^ bell time freq)
      evolvingDrops

{-
After 150 seconds (independent from the sample rate)
the sound stops but the program keeps running.
This suggests that this is not a problem of the signal generation.

This is independent from whether I run with
  +RTS -M32m -c30 -RTS
The sound is also stopped, when I just play a plain sine.

I can also reproduce this effect with the simple example
given in the Play module of my sox package.

But it cannot be 'sox's fault alone,
since playing a 180 second piece of music via pipe works:
sox Air.aiff -t sw - | play -r 44100 -t sw -c 2 -

When writing to a raw 'sw' format file this problem does not occur.
-}
{-# INLINE simpleStorable #-}
simpleStorable :: Proc.T s Dim.Time Double (Signal s Volt (Stereo.T Double))
simpleStorable =
   FiltA.amplify 0.5 $^
   (Cut.arrangeStorableVolume timeUnit (DN.voltage 1) timeUnit
--      $: chordSnds
--      $: evolvingDropSnds
      $: -- fmap (EventList.fromPairList . drop 1100 . EventList.toPairList)
            (liftA2 (EventList.mergeBy (\_ _ -> True))
               evolvingDropSnds
               (fmap (EventList.delay chordStartTicks) chordSnds)))

{-# INLINE simple #-}
simple ::
   Proc.T s Dim.Time Double
      (SigA.R s Dim.Voltage Double (Stereo.T Double))
simple =
   fmap SigA.restore simpleStorable


main :: IO ()
main =
{-
   Play.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (11025::Double))
--      (Cut.take (DN.time 2) $: simple)
      simple
     >> return ()
-}
{-
   Play.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (44100::Double))
      (Osci.static (DN.voltage 1 &*~ Wave.sine) zero (DN.frequency 880))
     >> return ()
-}
{-
   Play.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (44100::Double))
--      "rain.aiff"
      (Disp.mixMulti $::
          (strings (DN.time 10) (DN.frequency 440) :
           strings (DN.time 10) (DN.frequency 550) :
           strings (DN.time 10) (DN.frequency 660) :
           []))
     >> return ()
-}
{-
time ./dist/build/rain/rain +RTS -M128m -c30 -RTS

real    12m18.292s
user    12m7.389s
sys     0m1.668s
-}
   File.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (44100::Double))
--      "rain-long.aiff"
      "rain-short.aiff"
      ((CutA.dropWhile (DN.voltage 1) (zero==) .^
        Cut.take
           ((2 * NonNeg.toNumber partTicks +
             fromIntegral stringAttackTicks) *& timeUnit))
         $: simple)
     >> return ()
