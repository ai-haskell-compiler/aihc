{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Effect.Glass (glass) where

import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Number.NonNegative as NonNeg

import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Plain.Cut        as Cut
import qualified Synthesizer.Plain.Control    as Ctrl
import qualified Synthesizer.Plain.Noise      as Noise
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR

import System.Random(randomRs, mkStdGen)

import qualified Data.List.HT as ListHT

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealField      as RealField
import qualified Algebra.Module         as Module

import NumericPrelude.Numeric
import NumericPrelude.Base as NP


{- | We try to simulate the sound of broken glass
     as a mixture of short percussive sounds with random pitch -}
glass :: Double -> [Double]
glass sampleRate =
   Cut.arrange (particles sampleRate 1500)

particles :: Double -> Double -> EventList.T NonNeg.Int [Double]
particles sampleRate freq =
   let sampledDensity =
          (2000/sampleRate) *> map densityHeavy [0, (1/sampleRate) ..]
       pattern = take (round (0.8*sampleRate))
                      (Noise.randomPeeks sampledDensity)
       times   = timeDiffs pattern
       chirp   = iterate (0.001+) 0
       pitches = map ((freq*) . (2**))
                     (zipWith (+) chirp (randomRs (0,1) (mkStdGen 56)))
       amps    = map (0.4*) (map (2**) (randomRs (-2,0) (mkStdGen 721)))
   in  EventList.fromPairList $ zip times $
       zipWith (particle sampleRate) pitches amps


particle :: (RealField.C a, Trans.C a, Module.C a a) => a -> a -> a -> [a]
particle sampleRate freq amp =
   let halfLife = 0.01
   in  take (round (10*halfLife*sampleRate))
            (FiltNR.envelopeVector
                (Osci.static Wave.square 0 (freq/sampleRate))
                (Ctrl.exponential2 (0.01*sampleRate) amp))

_densitySmooth, densityHeavy :: Trans.C a => a -> a
_densitySmooth x = x * exp(-10*x*x)
densityHeavy  x = 0.4 * exp (-4*x)

_timeDiffs :: [Bool] -> [NonNeg.Int]
_timeDiffs =
   let diffs n (True  : xs) = n : diffs 1 xs
       diffs n (False : xs) = diffs (succ n) xs
       diffs _ [] = []
   in  diffs (NonNeg.fromNumber 0)

timeDiffs :: [Bool] -> [NonNeg.Int]
timeDiffs = map (NonNeg.fromNumber . length) . ListHT.segmentBefore id
