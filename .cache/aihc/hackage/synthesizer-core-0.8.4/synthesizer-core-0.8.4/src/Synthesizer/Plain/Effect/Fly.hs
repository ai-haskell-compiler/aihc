{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Plain.Effect.Fly where

import qualified Synthesizer.Causal.Spatial as Spatial
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Interpolation as Interpolation

import qualified Synthesizer.Plain.File as File
import System.Exit(ExitCode)

import System.Random (randomRs, mkStdGen, )

import qualified Algebra.NormedSpace.Euclidean as Euc

import NumericPrelude.Numeric
import NumericPrelude.Base


{-
  ghc -O -fvia-C --make Fly.hs && echo start && time a.out
-}

main :: IO ExitCode
main =
   File.writeStereoToInt16 "Fly" sampleRate
      (take (round (10*sampleRate)) fly)

sampleRate :: Double
sampleRate = 44100

{- | stereo sound of a humming fly -}
fly :: [(Double,Double)]
fly =
   let pinkNoise seed freq range =
           Interpolation.multiRelativeZeroPadCubic (0::Double)
           (repeat (freq/sampleRate))
           (randomRs (-range,range) (mkStdGen seed))
       {- the track of a fly is composed of a slow motion over a big range
          and fast but small oscillations -}
       flyCoord seed = zipWith (+) (pinkNoise seed 40 0.3)
                                   (pinkNoise seed  1 10)
       {- explicit signature required
          because of multi-param type class NormedEuc -}
       trajectory :: [(Double, Double, Double)]
       trajectory =
          zip3 (flyCoord 366210)
               (flyCoord 234298)
               (flyCoord 654891)

       channel ear =
          let (phase,volumes) =
                 unzip $ Causal.apply (Spatial.moveAround 1 0.1 ear) trajectory
              -- (*sampleRate) in 'speed' and
              -- (/sampleRate) in 'freqs' neutralizes
              speeds  = map (\v -> 250/sampleRate + 2 * Euc.norm v)
                            (FiltNR.differentiate trajectory)
              freqs   = zipWith (+) speeds (FiltNR.differentiate phase)
              sound   = Osci.freqModSaw 0 freqs
          in  zipWith (*) (map (10*) volumes) sound

   in  zip (channel (-1,0,0)) (channel (1,0,0))
