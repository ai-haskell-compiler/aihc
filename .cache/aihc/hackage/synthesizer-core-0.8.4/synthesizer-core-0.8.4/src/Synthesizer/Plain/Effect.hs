module Synthesizer.Plain.Effect where

import qualified Synthesizer.Plain.Noise as Noise
import qualified Synthesizer.Plain.Filter.Recursive as Filter
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder  as Filt1
import qualified Synthesizer.Plain.Filter.Recursive.Moog        as Moog
import qualified Synthesizer.Plain.Filter.Recursive.Comb        as Comb
import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butter
import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev   as Cheby
import Synthesizer.Plain.Control(exponential2)
import Synthesizer.Plain.Instrument
import Synthesizer.Plain.Effect.Glass(glass)

import qualified Synthesizer.Plain.File as File
import qualified Control.Monad.Exception.Synchronous as Exc
import System.Process (rawSystem, )
import System.Exit (ExitCode, )


main :: IO ExitCode
main =
   let rate = 44100
   in  Exc.toExitCodeT $
       do {- File.writeMono "test" rate
                (take (round (3*rate)) (soundD rate)) -}
          Exc.fromExitCodeT $ File.renderMonoToInt16 "test.aiff" rate soundE
          Exc.fromExitCodeT $ rawSystem "play" ["test.aiff"]


soundE, soundB, soundA,
   sound9, sound8, sound7,
   sound6, sound5, sound4,
   sound3, sound2, sound1,
   sound0, soundm0 :: Double -> [Double]

soundE = glass

{- moved to synthesizer-filter
soundD = flangedSaw
soundC _ = guitarRaw
-}

cFreq :: Double
cFreq = 521.3417849066773

soundB sampleRate =
   let baseFreq = cFreq/2
       chord = zipWith3 (\x y z -> (x+y+z)/5)
                        (choir sampleRate (baseFreq*1/1))
                        (choir sampleRate (baseFreq*5/4))
                        (choir sampleRate (baseFreq*3/2))
       filterFreqs = map (3000/sampleRate*)
                         (map sin (iterate (pi/(6*sampleRate)+) 0))
   in  Butter.lowpassPole 8 (repeat (0.3::Double)) filterFreqs (chord::[Double])

soundA sampleRate =
   choir sampleRate cFreq

sound9 sampleRate =
   map (*0.1) (accumulatedSaws sampleRate cFreq !! 20)

sound8 sampleRate =
   let filterFreqs = exponential2 (-0.5*sampleRate) (100/sampleRate)
   --  Cheby.lowpassBPole
   --  Cheby.highpassBPole
   --  Cheby.lowpassAPole
   --  Cheby.highpassAPole
   in  Cheby.lowpassBPole 8 (repeat (0.3::Double)) filterFreqs (Noise.white::[Double])

sound7 sampleRate =
   let filterFreqs = exponential2 (-0.5*sampleRate) (100/sampleRate)
   --  butterworthHighpass
   in  Butter.lowpassPole 8 (repeat (0.3::Double)) filterFreqs (Noise.white::[Double])

-- a moog filter which randomly changes the resonance frequency
sound6 sampleRate =
   let order = 10
       {- unused
       switchRates = repeat (8/sampleRate)
       filterFreqs = map (\exp -> 100*2**exp/sampleRate)
                         ((randomRs (0,6) (mkStdGen 142857))::[Double])
       filterReso  = 10
        -}

       control0 {-, control1, control2-} :: [Moog.Parameter Double]
       -- constant control
       control0 = repeat (Moog.parameter order (Filter.Pole 10 (400/sampleRate)))
       -- apply moogFilterParam first then resample, fast
       {- Need Additive and VectorSpace instances for MoogFilterParam
       control1 = interpolateConstant 0 switchRates
                     (map (moogFilterParam order)
                          (map (Pole filterReso) filterFreqs))
       -- first resample then apply moogFilterParam, slow
       control2 = map (moogFilterParam order)
                      (map (Pole filterReso)
                           (interpolateConstant 0 switchRates filterFreqs))
       -}
   in  Moog.lowpass order control0
          (map (0.5*) (fatSawChord sampleRate 220))

sound5 sampleRate =
   Comb.runMulti
      (map (\t -> round (t*sampleRate)) [0.08,0.13,0.21])
      (0.3::Double) (bell sampleRate 441)
sound4 sampleRate =
   Comb.runProc
      (round (0.3*sampleRate))
      (Filt1.lowpass
          (repeat (Filt1.parameter (441/sampleRate::Double))))
      (bell sampleRate 441)

sound3 sampleRate = allpassPlain sampleRate 0.3 1 441
sound2 sampleRate = allpassDown  sampleRate 10 0.5 1000 441

sound1 sampleRate = map (0.1*) (moogDown sampleRate 6 0.4 5000 441)
sound0 sampleRate = map (0.3*) (moogReso sampleRate 6 0.1 2000 441)

soundm0 sampleRate = fatSawChordFilter sampleRate 110
