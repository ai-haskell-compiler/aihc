module Test.Sound.Synthesizer.Generic.ToneModulation (tests) where

import Test.Sound.Synthesizer.Basic.ToneModulation (
   minLength,
   minLengthMargin,
--   shapeLimits,
--   testRationalLineIp,
   testRationalIp,
   )

import qualified Synthesizer.Causal.ToneModulation as ToneModC
import qualified Synthesizer.Generic.Wave as WaveG

import qualified Synthesizer.Plain.Signal         as Sig
import qualified Synthesizer.Plain.Oscillator     as Osci
import qualified Synthesizer.Plain.Interpolation  as Interpolation
import qualified Synthesizer.Plain.ToneModulation as ToneModL
import qualified Synthesizer.Plain.Wave   as WaveL
import Synthesizer.Interpolation (marginNumber, )

import qualified Synthesizer.Causal.Oscillator as OsciC
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.State.Signal as SigS

import qualified Synthesizer.Basic.Wave           as Wave
import qualified Synthesizer.Basic.Phase          as Phase

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty
import qualified Test.Sound.Synthesizer.Plain.Interpolation as InterpolationTest

import Test.QuickCheck (quickCheck, Property, (==>), )
import Test.Utility (ArbChar, )

import qualified Number.NonNegative       as NonNeg

import qualified Algebra.RealField             as RealField


import qualified Data.List as List
import Data.List.HT (viewL, takeWhileJust, )
import Data.Tuple.HT (mapSnd, )


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


limitMinRelativeValues ::
   Int -> Int -> [NonNeg.Int] -> Bool
limitMinRelativeValues xMin x0 xsnn =
   let xs = map NonNeg.toNumber xsnn
       (y0,limiter) = ToneModC.limitMinRelativeValues xMin x0
   in  (y0, Causal.apply limiter xs) ==
          ToneModL.limitMinRelativeValues xMin x0 xs

integrateFractional :: (RealField.C t) =>
   NonNeg.T t -> t -> Phase.T t -> [NonNeg.T t] -> [t] -> Property
integrateFractional
     periodNN shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       (c0, coordinator) =
          ToneModC.integrateFractional
             period (shape0, phase)
       coords =
          ToneModL.integrateFractional
             period (shape0, shapes) (phase, freqs)
   in  period /= zero  ==>
          c0 : Causal.apply coordinator (zip shapes freqs) ==
          coords

-- oscillatorCellSize :: (Show t, Show v, RealField.C t, Eq v) =>
oscillatorCellSize :: (RealField.C t, Eq v) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   NonNeg.Int -> NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] ->
   Property
oscillatorCellSize
      marginLeap marginStep periodIntNN periodNN ext
      ixs shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       len = minLengthMargin marginLeap marginStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledTone =
          ToneModC.oscillatorCells
             marginLeap marginStep periodInt period tone
             (shape0, Phase.fromRepresentative phase)
          `Causal.apply`
          zip shapes freqs
   in  period /= zero  &&
       marginNumber marginLeap > zero &&
       marginNumber marginStep > zero  ==>
       all
          ((\cell ->
              Sig.lengthAtLeast (marginNumber marginLeap) cell &&
              all (Sig.lengthAtLeast (marginNumber marginStep))
                  (take (marginNumber marginLeap) cell))
           . SigS.toList . snd)
          resampledTone

oscillatorSuffixes :: (RealField.C t, Eq v) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   NonNeg.Int -> NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] ->
   Property
oscillatorSuffixes
      marginLeap marginStep periodIntNN periodNN ext
      ixs shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       len = minLengthMargin marginLeap marginStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledToneA =
          init $
          map (\(sp,cell) ->
             (sp, takeWhileJust . map (fmap fst . viewL) $ cell)) $
          ToneModL.oscillatorSuffixes
             marginLeap marginStep periodInt period tone
             (shape0, shapes) (Phase.fromRepresentative phase, freqs)
       resampledToneB =
          ToneModC.oscillatorSuffixes
             marginLeap marginStep periodInt period tone
             (shape0, Phase.fromRepresentative phase)
          `Causal.apply`
          zip shapes freqs
   in  period /= zero  &&
       periodInt /= zero  &&
       marginNumber marginLeap > zero &&
       marginNumber marginStep > zero  ==>
          resampledToneA == resampledToneB

oscillatorCells :: (RealField.C t, Eq v) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   NonNeg.Int -> NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] ->
   Property
oscillatorCells
      marginLeap marginStep periodIntNN periodNN ext
      ixs shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       len = minLengthMargin marginLeap marginStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledToneA =
          init $ map (mapSnd List.transpose) $
          ToneModL.oscillatorCells
             marginLeap marginStep periodInt period tone
             (shape0, shapes) (Phase.fromRepresentative phase, freqs)
       resampledToneB =
          map (mapSnd SigS.toList) $
          ToneModC.oscillatorCells
             marginLeap marginStep periodInt period tone
             (shape0, Phase.fromRepresentative phase)
          `Causal.apply`
          zip shapes freqs
   in  period /= zero  &&
       periodInt /= zero  &&
       marginNumber marginLeap > zero &&
       marginNumber marginStep > zero  ==>
          resampledToneA == resampledToneB
{-
Margin {marginNumber = 1, marginOffset = 2}
Margin {marginNumber = 5, marginOffset = 5}
3 % 4
0
('\DEL',['~','~','"'])
-2 % 5
2 % 5
[2 % 1,3 % 4]
[-5 % 2,-1 % 2]
-}

{- |
'WaveL.sampledTone' and 'WaveG.sampledTone'
do not only differ in the signal types they process,
but also in the way they order the signal values.
The cells for 'WaveL.sampledTone' are transposed
with respect to 'WaveG.sampledTone'.
-}
sampledTone :: (RealField.C a, Eq v) =>
   InterpolationTest.T a v ->
   InterpolationTest.T a v ->
   NonNeg.T a -> NonNeg.Int -> NonEmpty.T v ->
   a -> Phase.T a -> Property
sampledTone =
   InterpolationTest.use2 $ \ ipLeap ipStep
         periodNN ext ixs shape phase ->
   let period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
   in  period /= zero ==>
          WaveG.sampledTone ipLeap ipStep period tone shape `Wave.apply` phase ==
          WaveL.sampledTone ipLeap ipStep period tone shape `Wave.apply` phase



shapeFreqModFromSampledTone :: (RealField.C t, Eq v) =>
   InterpolationTest.T t v ->
   InterpolationTest.T t v ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> Phase.T t -> [NonNeg.T t] -> [t] ->
   Property
shapeFreqModFromSampledTone =
   InterpolationTest.use2 $ \ ipLeap ipStep
         periodNN ext ixs shape0 phase shapesNN freqs ->
   let shapes = map NonNeg.toNumber shapesNN
       period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledToneA =
          init $
          Osci.shapeFreqModFromSampledTone ipLeap ipStep period tone
             shape0 (Phase.toRepresentative phase) shapes freqs
       resampledToneB =
          OsciC.shapeFreqModFromSampledTone
             ipLeap ipStep period tone shape0 phase
          `Causal.apply`
          zip shapes freqs
   in  period /= zero  ==>
          resampledToneA == resampledToneB


{-
We have a problem here with the phase distortion signal,
because frequency and shape modulation control signals
are delayed by one element with respect to the phase distortion.
How can we cope with different lengths of the control signals,
without padding the phase control with zeros?
This one did not work:
   phaseDistorts = pd:pds
   resampledToneA =
      Osci.shapePhaseFreqModFromSampledTone ipLeap ipStep period tone
         shape0 (Phase.toRepresentative phase) shapes (init phaseDistorts) freqs
-}
shapePhaseFreqModFromSampledTone :: (RealField.C t, Eq v) =>
   InterpolationTest.T t v ->
   InterpolationTest.T t v ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> Phase.T t -> [NonNeg.T t] -> (t,[t]) -> [t] ->
   Property
shapePhaseFreqModFromSampledTone =
   InterpolationTest.use2 $ \ ipLeap ipStep
         periodNN ext ixs shape0 phase shapesNN (pd,pds) freqs ->
   let period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       shapes = map NonNeg.toNumber shapesNN
       phaseDistorts = pd:pds ++ repeat zero
       resampledToneA =
          init $
          Osci.shapePhaseFreqModFromSampledTone ipLeap ipStep period tone
             shape0 (Phase.toRepresentative phase) shapes phaseDistorts freqs
       resampledToneB =
          OsciC.shapePhaseFreqModFromSampledTone
             ipLeap ipStep period tone shape0 phase
          `Causal.apply`
          zip3 shapes phaseDistorts freqs
   in  period /= zero  ==>
          resampledToneA == resampledToneB



tests :: [(String, IO ())]
tests =
   ("limitMinRelativeValues", quickCheck limitMinRelativeValues) :
   ("integrateFractional",
      quickCheck (\period -> integrateFractional (period :: NonNeg.Rational))) :
   ("oscillatorCellSize",
      quickCheck (\ml ms periodInt period ext ixs ->
               oscillatorCellSize ml ms periodInt (period :: NonNeg.Rational)
                  ext (ixs :: NonEmpty.T ArbChar))) :
   ("oscillatorSuffixes",
      quickCheck (\ml ms periodInt period ext ixs ->
               oscillatorSuffixes ml ms periodInt (period :: NonNeg.Rational)
                  ext (ixs :: NonEmpty.T ArbChar))) :
   ("oscillatorCells",
      quickCheck (\ml ms periodInt period ext ixs ->
               oscillatorCells ml ms periodInt (period :: NonNeg.Rational)
                  ext (ixs :: NonEmpty.T ArbChar))) :
   ("sampledTone",
      testRationalIp sampledTone) :
   ("shapeFreqModFromSampledTone",
      testRationalIp shapeFreqModFromSampledTone) :
   ("shapePhaseFreqModFromSampledTone",
      testRationalIp shapePhaseFreqModFromSampledTone) :
   []
