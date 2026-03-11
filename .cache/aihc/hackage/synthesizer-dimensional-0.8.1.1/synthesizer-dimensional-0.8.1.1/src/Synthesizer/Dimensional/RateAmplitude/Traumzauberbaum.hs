{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where
-- module Synthesizer.Dimensional.RateAmplitude.Traumzauberbaum where

-- import qualified Synthesizer.Dimensional.RateAmplitude.Instrument as Instr

import qualified Synthesizer.Dimensional.Rate.Oscillator as Osci
import qualified Synthesizer.Dimensional.Rate.Filter     as Filt
import qualified Synthesizer.Dimensional.RateAmplitude.Displacement as Disp
import qualified Synthesizer.Dimensional.RateAmplitude.Noise      as Noise
import qualified Synthesizer.Dimensional.RateAmplitude.Filter     as FiltA
import qualified Synthesizer.Dimensional.RateAmplitude.Cut        as Cut
-- import qualified Synthesizer.Dimensional.Amplitude.Cut            as CutA

import qualified Synthesizer.Dimensional.RateAmplitude.Control    as Ctrl
-- import qualified Synthesizer.Dimensional.Rate.Control             as CtrlR

import qualified Synthesizer.Dimensional.Wave as WaveD

import Synthesizer.Dimensional.Wave ((&*~), )

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal as SigA

import qualified Synthesizer.Dimensional.RateAmplitude.File as File
import qualified Synthesizer.Dimensional.RateAmplitude.Play as Play

import qualified Data.StorableVector.Lazy.Builder as Bld
import Data.Int (Int16, )

import Synthesizer.Dimensional.Signal (($-), )
import Synthesizer.Dimensional.Process (($:), ($::), ($^), )
import Synthesizer.Dimensional.Amplitude.Displacement (mapExponential, )

import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Frame.Stereo as Stereo

-- import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.Basic.Wave as Wave

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import Number.DimensionTerm ((*&))

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

melody :: [(Pitch, Int)]
melody =
   ((g,4),4) : ((g,4),2) : ((c,4),4) : ((d,4),2) : ((e,4),12) :
   ((g,4),4) : ((g,4),2) : ((c,4),4) : ((d,4),2) : ((e,4),12) :
   ((c,4),4) : ((c,4),2) : ((d,4),4) : ((d,4),2) : ((e,4),12) :
   ((c,4),4) : ((c,4),2) : ((d,4),4) : ((d,4),2) : ((e,4),12) :
   ((a,4),4) : ((a,4),2) : ((f,4),4) : ((f,4),2) : ((d,4),12) :
   ((g,4),4) : ((g,4),2) : ((c,4),4) : ((d,4),2) : ((e,4),12) :
   ((a,4),4) : ((a,4),2) : ((g,4),4) : ((g,4),2) : ((f,4),12) :
   ((g,4),4) : ((g,4),2) : ((c,4),4) : ((d,4),2) : ((c,4),12) :
   []


type Chord = [Pitch]

chords :: [(Chord, Int)]
chords =
   ([(c,4),(e,4),(g,4)],  6) :
   ([(a,3),(c,4),(f,4)],  4) :
   ([(g,3),(h,3),(d,4)],  2) :
   ([(g,3),(c,4),(e,4)], 12) :

   ([(c,4),(e,4),(g,4)],  6) :
   ([(a,3),(c,4),(f,4)],  4) :
   ([(g,3),(h,3),(d,4)],  2) :
   ([(g,3),(c,4),(e,4)], 12) :

   ([(a,3),(c,4),(e,4)],  6) :
   ([(g,3),(h,3),(d,4)],  6) :
   ([(g,3),(c,4),(e,4)], 12) :

   ([(a,3),(c,4),(e,4)],  6) :
   ([(g,3),(h,3),(d,4)],  6) :
   ([(g,3),(c,4),(e,4)], 12) :

   ([(a,3),(c,4),(f,4)],  6) :
   ([(a,3),(d,4),(f,4)],  6) :
   ([(g,3),(h,3),(d,4)], 12) :

   ([(c,4),(e,4),(g,4)],  6) :
   ([(a,3),(c,4),(f,4)],  4) :
   ([(g,3),(h,3),(d,4)],  2) :
   ([(g,3),(c,4),(e,4)], 12) :

   ([(a,3),(c,4),(f,4)],  6) :
   ([(g,3),(h,3),(e,4)],  6) :
   ([(f,3),(a,3),(d,4)], 12) :

   ([(c,4),(e,4),(g,4)],  6) :
   ([(a,3),(c,4),(f,4)],  4) :
   ([(g,3),(h,3),(d,4)],  2) :
   ([(e,3),(g,3),(c,4)], 12) :

   []


bass :: [(Pitch, Int)]
bass =
   ((c,5), 6) : ((f,4), 4) : ((g,4), 2) : ((c,5), 12) :
   ((c,5), 6) : ((f,4), 4) : ((g,4), 2) : ((c,5), 12) :
   ((a,4), 4) : ((a,4), 2) : ((g,4), 4) : ((g,4),  2) : ((c,5), 12) :
   ((a,4), 4) : ((a,4), 2) : ((g,4), 4) : ((g,4),  2) : ((c,5), 12) :
   ((f,4), 4) : ((f,4), 2) : ((d,4), 4) : ((d,4),  2) : ((g,4), 12) :
   ((c,5), 6) : ((f,4), 4) : ((g,4), 2) : ((c,5), 12) :
   ((f,5), 6) : ((e,5), 6) : ((d,5), 12) :
   ((c,5), 6) : ((f,4), 4) : ((g,4), 2) : ((c,4), 12) :
   []


harmony :: [Pitch]
harmony =
   (c,4) : (g,4) : (c,5) : (f,3) : (c,4) : (g,3) :
   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :
   (c,4) : (g,4) : (c,5) : (f,3) : (c,4) : (g,3) :
   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :

   (a,3) : (e,4) : (a,4) : (g,3) : (d,4) : (g,4) :
   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :
   (a,3) : (e,4) : (a,4) : (g,3) : (d,4) : (g,4) :
   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :

   (f,3) : (c,4) : (f,4) : (a,3) : (d,4) : (a,4) :
   (g,3) : (d,4) : (g,4) : (g,3) : (d,4) : (g,4) :
   (c,4) : (g,4) : (c,5) : (f,3) : (c,4) : (g,3) :
   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :

   (f,3) : (c,4) : (f,4) : (e,3) : (h,3) : (e,4) :
   (d,3) : (a,3) : (d,4) : (a,3) : (d,4) : (a,4) :
   (c,4) : (g,4) : (c,5) : (f,3) : (c,4) : (g,3) :
   (c,4) : (g,4) : (c,5) : (c,4) : (c,4) : (c,4) :
--   (c,4) : (g,4) : (c,5) : (c,4) : (g,4) : (c,5) :

   []



{-# INLINE assemblePitch #-}
assemblePitch :: Pitch -> Double
assemblePitch (pc, oct) =
   -- this contains a transposition by a third and four octaves
   fromIntegral pc / 12 + fromIntegral oct - 4

{-# INLINE smoothSaw #-}
smoothSaw ::
   Double ->
   WaveD.T Double (Sample.Dimensional Dim.Voltage Double Double)
smoothSaw p =
   DN.voltage 1 &*~ Wave.triangleAsymmetric p

{-# INLINE smoothSquare #-}
smoothSquare ::
   Double ->
   WaveD.T Double (Sample.Dimensional Dim.Voltage Double Double)
smoothSquare p =
   DN.voltage 1 &*~ Wave.trapezoid p


{-# INLINE timeUnit #-}
timeUnit :: DN.T Dim.Time Double
timeUnit = DN.time 0.2

{-# INLINE pitchControl #-}
pitchControl ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Scalar Double Double)
--   Proc.T s Dim.Time Double (SigS.R s Double)
pitchControl =
   Cut.concatVolume (DN.scalar 1) $:
   (mapM (\(p,dur) ->
      Cut.take (fromIntegral dur *& timeUnit)
       $: Ctrl.constant (DN.scalar (assemblePitch p))) melody)


{-# INLINE _simpleMusic #-}
_simpleMusic ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
_simpleMusic =
   Osci.freqMod (smoothSquare 0.9) zero
      $: (mapExponential 2 (DN.frequency 440) $^ pitchControl)


{-# INLINE filteredPitchControl #-}
filteredPitchControl ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Scalar Double Double)
filteredPitchControl =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 3
         $- DN.frequency 4
         $: pitchControl)


{-# INLINE envelope #-}
envelope ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Scalar Double Double)
envelope =
   Filt.firstOrderLowpass
      $- DN.frequency 10
      $: (Filt.firstOrderHighpass
             $- DN.frequency 0.3
             $: pitchControl)


{-# INLINE envelopedMelody #-}
envelopedMelody ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
envelopedMelody =
   Filt.envelope $: envelope $:
    (Osci.freqMod (smoothSquare 0.9) zero
       $: (mapExponential 2 (DN.frequency 440) $^ filteredPitchControl))


{-# INLINE _filteredMusic #-}
_filteredMusic ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)
_filteredMusic =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 10
         $: (mapExponential 20 (DN.frequency 100) $^ envelope)
         $: (Osci.freqMod (smoothSquare 0.9) zero
               $: (mapExponential 2 (DN.frequency 440) $^ pitchControl)))


{-# INLINE _makeChordPhaser #-}
_makeChordPhaser ::
   Chord ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
_makeChordPhaser chord =
   Disp.mixMulti $::
   (map (\p ->
       Cut.mergeStereo
          $: Osci.static (smoothSaw 0.9) zero
                 (2 ** assemblePitch p *& DN.frequency 439)
          $: Osci.static (smoothSaw 0.9) zero
                 (2 ** assemblePitch p *& DN.frequency 441))
       chord)

{-# INLINE makeChord #-}
makeChord ::
   Chord ->
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
makeChord chord =
   Disp.mixMulti $::
   (map (\p ->
       let {-# INLINE tone #-}
           tone noise =
              Osci.freqMod (smoothSaw 0.9) zero $:
--              (Osci.freqMod (Wave.saw) zero $:
                (mapExponential 2 (DN.frequency 440) $^
                    (Disp.raise (DN.scalar (assemblePitch p)) $:
                       (Filt.firstOrderLowpass
                           $- DN.frequency 2
                           $: noise)))
{-
       in Cut.mergeStereo
             $: (tone (Ctrl.constant (DN.scalar 0.01)))
             $: (tone (Ctrl.constant (DN.scalar (-0.01)))))
-}
{-
       in Cut.mergeStereo
             $: (tone                (Noise.white (DN.frequency 10000) (DN.scalar 0.5)))
             $: (tone (Filt.negate $: Noise.white (DN.frequency 10000) (DN.scalar 0.5))))
-}
       in SigA.share
             (Noise.white (DN.frequency 10000) (DN.scalar 0.5))
             (\ns ->
                Cut.mergeStereo
                   $: (tone ns)
                   $: (tone (Filt.negate $: ns))))
{-
       in Cut.mergeStereo
             $: (tone (Noise.white (DN.frequency 10000) (DN.scalar 0.5)))
             $: (tone (Ctrl.constant (DN.scalar (-0.02)))))
-}
{-
       in Cut.mergeStereo
             $: (tone (Osci.static (DN.scalar   1  &*~ Wave.sine) zero (DN.frequency 3)))
             $: (tone (Osci.static (DN.scalar (-1) &*~ Wave.sine) zero (DN.frequency 3))))
-}
       chord)

{-# INLINE chordAccompaniment #-}
chordAccompaniment ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
chordAccompaniment =
   Cut.concat $::
   (map (\(chd,dur) -> Cut.take (fromIntegral dur *& timeUnit) $: makeChord chd) chords)



{-# INLINE bassControl #-}
bassControl ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Scalar Double Double)
--   Proc.T s Dim.Time Double (SigS.R s Double)
bassControl =
   Cut.concatVolume (DN.scalar 1) $::
   (map (\(p,dur) ->
      Cut.take (fromIntegral dur *& timeUnit)
       $: Ctrl.constant (DN.scalar (assemblePitch p))) bass)
{-
   Cut.concatVolume (DN.scalar 1) $:
   (mapM (\(p,dur) ->
      Cut.take (fromIntegral dur *& timeUnit)
       $: Ctrl.constant (DN.scalar (assemblePitch p))) bass)
-}

{-# INLINE _bassPhaserSignal #-}
_bassPhaserSignal ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
_bassPhaserSignal =
   Cut.mergeStereo
      $: (Osci.freqMod (smoothSaw 0.8) zero $:
            (mapExponential 2 (DN.frequency 54.7) $^ bassControl))
      $: (Osci.freqMod (smoothSaw 0.8) zero $:
            (mapExponential 2 (DN.frequency 55.3) $^ bassControl))

{-# INLINE bassSignal #-}
bassSignal ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
bassSignal =
{-
   SigA.share
      (Osci.freqMod (smoothSaw 0.9) zero $:
             (mapExponential 2 (DN.frequency 110) $^ bassControl))
      (\b -> Cut.mergeStereo $: b $: b)
-}
{-
   SigA.share
      bassControl
      (\b ->
          let {-# INLINE channel #-}
              channel p =
                 Osci.freqMod (smoothSaw 0.9) zero $: p
          in  Cut.mergeStereo
                 $: channel (mapExponential 2 (DN.frequency 109.7) $^ b)
                 $: channel (mapExponential 2 (DN.frequency 110.3) $^ b))
-}
{-
   SigA.share
      bassControl
      (\b ->
         Filt.envelopeVector
            $: (Osci.freqMod ((1+) . Wave.triangleAsymmetric 0.9) zero $:
                  (mapExponential 2 (DN.frequency 27.5) $^ b))
            $: (Cut.mergeStereo
                  $: (Osci.freqMod (smoothSaw 0.9) zero $:
                        (mapExponential 2 (DN.frequency 109.7) $^ b))
                  $: (Osci.freqMod (smoothSaw 0.9) zero $:
                        (mapExponential 2 (DN.frequency 110.3) $^ b))))
-}
   SigA.share
      (Filt.firstOrderLowpass $- DN.frequency 2 $: bassControl)
      (\b ->
         Filt.envelopeVector
            $: (Osci.freqMod
                  (WaveD.flat $ Wave.raise one $ Wave.triangleAsymmetric 0.9) zero $:
                  (mapExponential 2 (DN.frequency 27.5) $^ b))
            $: (let {-# INLINE channel #-}
                    channel p =
                       Osci.freqMod (smoothSaw 0.9) zero $: p
                in  Cut.mergeStereo
                       $: channel (mapExponential 2 (DN.frequency 109.7) $^ b)
                       $: channel (mapExponential 2 (DN.frequency 110.3) $^ b)))


{-# INLINE accompaniment #-}
accompaniment ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
accompaniment =
   Disp.mix
      $: (FiltA.amplify 0.3 $: bassSignal)
      $: (FiltA.amplify 0.1 $: chordAccompaniment)
{-
   FiltA.amplify 0.1 $: chordAccompaniment
-}
{-
   FiltA.amplify 0.3 $: bassSignal
-}


{-# INLINE filteredAccompaniment #-}
filteredAccompaniment ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
filteredAccompaniment =
   Filt.lowpassFromUniversal $^
      (Filt.universal
         $- DN.scalar 5
         $: (mapExponential 2 (DN.frequency 440) $^
               (Cut.concatVolume (DN.scalar 1) $:
                   (mapM (\p ->
                      Cut.take (2 *& timeUnit)
                         $: Ctrl.constant (DN.scalar (assemblePitch p))) harmony)))
         $: accompaniment)




{-# INLINE songSignal #-}
songSignal ::
   Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))
songSignal =
   Disp.mixMulti $::
      (FiltA.amplify 0.5 $:
         SigA.share envelopedMelody (\m -> Cut.mergeStereo $: m $: m)) :
      (FiltA.amplify 0.3 $: filteredAccompaniment) :
      []


_playBuilder :: IO ()
_playBuilder =
   Play.renderTimeVoltage
      (Bld.put :: Int16 -> Bld.Builder Int16)
      (DN.frequency (44100::Double))
      songSignal
     >> return ()

_render :: IO ()
_render =
   File.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (44100::Double))
      "traumzauberbaum.aiff"
      songSignal
     >> return ()

main :: IO ()
main =
   Play.renderTimeVoltageStereoDoubleToInt16
      (DN.frequency (44100::Double))
--      (Cut.take (DN.time 2) $: songSignal)
      songSignal
--      accompaniment
--      bassSignal
     >> return ()

{-
import installed synthesizer package

ghc -o dist/build/traumzauberbaum/traumzauberbaum -O -Wall -fexcess-precision -ddump-simpl-stats -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Traumzauberbaum.hs

ghc -o dist/build/traumzauberbaum/traumzauberbaum-prof -prof -auto-all -O -Wall -fexcess-precision -ddump-simpl-stats -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Traumzauberbaum.hs

ghc -o dist/build/traumzauberbaum/traumzauberbaum -O -Wall -fexcess-precision -ddump-simpl-iterations -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Traumzauberbaum.hs >dist/build/Traumzauberbaum.log

ghc-core -f html -- -o dist/build/traumzauberbaum/traumzauberbaum -O -Wall -fexcess-precision -fvia-C -optc-O2 -package synthesizer src/Synthesizer/Dimensional/RateAmplitude/Traumzauberbaum.hs >dist/build/traumzauberbaum/traumzauberbaum.html
-}
