{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Filter.Recursive.Test where

import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2
import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butter
import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev   as Cheby
import qualified Synthesizer.Plain.Filter.Recursive.Moog        as Moog
import qualified Synthesizer.Plain.Filter.Recursive.Universal   as Uni
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrderComplex as C1
import qualified Synthesizer.Basic.Wave as Wave

import Synthesizer.Plain.Filter.Recursive (Pole(..))

import Number.Complex ((+:), real, imag, )
import qualified Number.Complex as Complex

import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Ring                as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


sampleRate :: Ring.C a => a
--sampleRate = 44100
sampleRate = 22050
--sampleRate = 11025


chirp :: Double -> [Double]
chirp len = Osci.freqModSine 0 (iterate (+0.5/len) 0)

filter2ndOrderTest :: [Double]
filter2ndOrderTest =
   take 10
      (Filt2.run
          (repeat (Filt2.Parameter 1 0 0 0 (1::Double)))
          (1 : repeat 0))


butterworthLowpassTest0 :: [Double]
butterworthLowpassTest0 =
   take 30 (Butter.lowpassPole 2 (repeat 0.2) (repeat (0.1::Double)) (repeat 1))

butterworthLowpassTest1 :: Double
butterworthLowpassTest1 =
   maximum (take 300 (drop 500
         (Butter.lowpassPole 6 (repeat 0.1) (repeat (0.05::Double))
               (map sin (iterate (+2*pi*0.05) 0)))))

butterworthLowpassTest2 :: [Double]
butterworthLowpassTest2 =
   let len = 1*sampleRate
   in  take (round len) (Butter.lowpassPole 20 (repeat 0.3) (repeat (0.2::Double)) (chirp len))

chebyParameterA, chebyParameterB :: (Trans.C a) =>
   a -> Complex.T a -> a -> Filt2.Parameter a
chebyParameterA vol z freq =
   let re      = real z
       im      = imag z
       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       cpims   = cosphi + im*sinphi
       cmims   = cosphi - im*sinphi
       resin2  = (re*sinphi)^2
       denom   = - cmims^2 - resin2
       c0      = vol * sinphi^2 / denom
   in  Filt2.Parameter
          c0 (2*c0) c0
          (-2*(cpims*cmims - resin2)/denom) ((cpims^2 + resin2)/denom)

chebyParameterB a0 z freq =
   let re      = real z
       im      = imag z
       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       spimc   = sinphi + im*cosphi
       smimc   = sinphi - im*cosphi
       recos2  = (re*cosphi)^2
       denom   = smimc^2 + recos2
       c0      = (sinphi^2 + a0^2*cosphi^2) / denom
       c1      = (sinphi^2 - a0^2*cosphi^2) / denom
   in  Filt2.Parameter
          c0 (2*c1) c0
          (-2*(spimc*smimc - recos2)/denom) (-(spimc^2 + recos2)/denom)

-- cf. makeZero
chebyshevALowpassTest0 :: Filt2.Parameter Double
chebyshevALowpassTest0 =
   let beta = asinh 1 / 4
   in  chebyParameterA 1 (12/13 * cosh beta +: (-5/13 * sinh beta)) 0.1

chebyshevBLowpassTest0 :: Filt2.Parameter Double
chebyshevBLowpassTest0 =
   let beta = asinh 1 / 4
   in  chebyParameterB (12/13) (12/13 * cosh beta +: (-5/13 * sinh beta)) 0.1

chebyshevLowpassTest1 :: [Double]
chebyshevLowpassTest1 =
   let len = 1*sampleRate
   in  take (round len) (Filt2.run (repeat chebyshevALowpassTest0) (chirp len))

chebyshevALowpassTest2 :: [Double]
chebyshevALowpassTest2 =
   let len = 1*sampleRate
   in  take (round len) $
       Cheby.lowpassAPole 10 (repeat 0.25) (repeat (0.3::Double)) (chirp len)

chebyshevBLowpassTest2 :: [Double]
chebyshevBLowpassTest2 =
   let len = 1*sampleRate
   in  take (round len) $
       Cheby.lowpassBPole 10 (repeat 0.25) (repeat (0.1::Double)) (chirp len)



moogLowpassTest :: [Double]
moogLowpassTest =
   Moog.lowpass 10
      (repeat (Moog.parameter 10 (Pole 10 (0.05::Double))))
      (1:repeat 0)

universalTest :: [Uni.Result Double]
universalTest =
   let len = 1*sampleRate
   in  take (round len) $
       Uni.run
          (repeat (Uni.parameter (Pole 5 (0.1::Double))))
          (chirp len)


complexRealTest :: [Complex.T Double]
complexRealTest =
   let len = 1*sampleRate
   in  take (round len) $
       C1.run
          (repeat (C1.parameterFromPeakWidth 0.025 (Pole 5 (0.1::Double))))
          (chirp len)

chirpComplex :: Double -> [Complex.T Double]
chirpComplex len =
   Osci.freqMod Wave.helix 0 (iterate (+0.5/len) 0)

complexTest :: [Complex.T Double]
complexTest =
   let len = 1*sampleRate
   in  take (round len) $
       map
          (\x -> Complex.real x + Complex.quarterLeft (Complex.imag x)) $
       C1.run
          (repeat (C1.parameterFromPeakWidth 0.025 (Pole 5 (0.1::Double))))
          (chirpComplex len)
