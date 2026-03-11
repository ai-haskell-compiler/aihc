{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.Plain.Instrument where

import Synthesizer.Plain.Displacement (mixMulti, )
import Synthesizer.Plain.Control (exponential2)
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Plain.Noise      as Noise
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1
import qualified Synthesizer.Plain.Filter.Recursive.Allpass    as Allpass
import qualified Synthesizer.Plain.Filter.Recursive.Universal  as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive.Moog       as Moog
import qualified Synthesizer.Plain.Filter.Recursive.Comb       as Comb
import qualified Synthesizer.Plain.Filter.Recursive    as FiltR
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Interpolation as Interpolation
import Data.List(zipWith4)

import System.Random

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base



{-| Create a sound of a slightly changed frequency
    just as needed for a simple stereo sound. -}
stereoPhaser :: Ring.C a =>
       (a -> [b])  {- ^ A function mapping a frequency to a signal. -}
    -> a           {- ^ The factor to the frequency, should be close to 1. -}
    -> a           {- ^ The base (undeviated) frequency of the sound. -}
    -> [b]
stereoPhaser sound dif freq = sound (freq*dif)



allpassPlain :: (RealField.C a, Trans.C a, Module.C a a) =>
                   a -> a -> a -> a -> [a]
allpassPlain sampleRate halfLife k freq =
    Allpass.cascade 10
        (map Allpass.Parameter (exponential2 (halfLife*sampleRate) k))
        (simpleSaw sampleRate freq)

allpassDown :: (RealField.C a, Trans.C a, Module.C a a) =>
                  a -> Int -> a -> a -> a -> [a]
allpassDown sampleRate order halfLife filterfreq freq =
    let x = simpleSaw sampleRate freq
    in  map (0.3*) (zipWith (+) x
            (Allpass.cascade order
                (map (Allpass.flangerParameter order)
                     (exponential2 (halfLife*sampleRate) (filterfreq/sampleRate)))
                x))


moogDown, moogReso ::
   (RealField.C a, Trans.C a, Module.C a a) =>
      a -> Int -> a -> a -> a -> [a]
moogDown sampleRate order halfLife filterfreq freq =
    Moog.lowpass order
        (map (Moog.parameter order) (map (FiltR.Pole 10)
            (exponential2 (halfLife*sampleRate) (filterfreq/sampleRate))))
        (simpleSaw sampleRate freq)

moogReso sampleRate order halfLife filterfreq freq =
    Moog.lowpass order
        (map (Moog.parameter order) (zipWith FiltR.Pole
            (exponential2 (halfLife*sampleRate) 100)
            (repeat (filterfreq/sampleRate))))
        (simpleSaw sampleRate freq)

bell :: (Trans.C a, RealField.C a) => a -> a -> [a]
bell sampleRate freq =
    let halfLife = 0.5
    in  zipWith3 (\x y z -> (x+y+z)/3)
            (bellHarmonic sampleRate 1 halfLife freq)
            (bellHarmonic sampleRate 4 halfLife freq)
            (bellHarmonic sampleRate 7 halfLife freq)

bellHarmonic :: (Trans.C a, RealField.C a) => a -> a -> a -> a -> [a]
bellHarmonic sampleRate n halfLife freq =
    zipWith (*) (Osci.freqModSine 0 (map (\modu -> freq/sampleRate*n*(1+0.005*modu))
                                    (Osci.staticSine 0 (5.0/sampleRate))))
                (exponential2 (halfLife/n*sampleRate) 1)


fastBell, squareBell, moogGuitar, moogGuitarSoft, simpleSaw, fatSaw ::
    (RealField.C a, Trans.C a, Module.C a a) => a -> a -> [a]

fastBell sampleRate freq =
    zipWith (*) (Osci.staticSine 0 (freq/sampleRate))
                (exponential2 (0.2*sampleRate) 1)

filterSaw :: (Module.C a a, Trans.C a, RealField.C a) =>
             a -> a -> a -> [a]
filterSaw sampleRate filterFreq freq =
    map (\r -> UniFilter.lowpass r * 0.1)
        (UniFilter.run (map (UniFilter.parameter . FiltR.Pole 10)
                        (exponential2 (0.1*sampleRate) (filterFreq/sampleRate)))
                   (Osci.staticSaw 0 (freq/sampleRate)))

squareBell sampleRate freq = Filt1.lowpass
         (map Filt1.parameter
              (exponential2 (sampleRate/10) (4000/sampleRate)))
--       (Osci.freqModSample Interpolation.cubic [0, 0.7, -0.3, 0.7, 0, -0.7, 0.3, -0.7] 0
         (Osci.freqModSample Interpolation.linear [0, 0.5, 0.6, 0.8, 0, -0.5, -0.6, -0.8] 0
                  (map (\modu -> freq/sampleRate*(1+modu/100))
                       (Osci.staticSine 0 (5.0/sampleRate))))

fmBell :: (RealField.C a, Trans.C a) => a -> a -> a -> a -> [a]
fmBell sampleRate depth freqRatio freq =
   let modul = FiltNR.envelope (exponential2 (0.2*sampleRate) depth)
                        (Osci.staticSine 0 (freqRatio*freq/sampleRate))
       env   = exponential2 (0.5*sampleRate) 1
   in  FiltNR.envelope env (Osci.phaseModSine (freq/sampleRate) modul)

moogGuitar sampleRate freq =
   let moogOrder = 4
       filterControl =
          map (Moog.parameter moogOrder)
              (map (FiltR.Pole 10) (exponential2
                               (0.5*sampleRate)
                               (4000/sampleRate)))
       tone = Osci.freqModSaw 0 (map (\modu -> freq/sampleRate*(1+0.005*modu))
                                (Osci.staticSine 0 (5.0/sampleRate)))
   in  Moog.lowpass moogOrder filterControl tone

moogGuitarSoft sampleRate freq =
   FiltNR.envelope (map (1-) (exponential2 (0.003*sampleRate) 1))
            (moogGuitar sampleRate freq)



{-| low pass with resonance -}
filterSweep :: (Field.C v, Module.C a v, Trans.C a, RealField.C a) =>
                  a -> a -> [v] -> [v]
filterSweep sampleRate phase =
    map (\r -> UniFilter.lowpass r / 2) .
    UniFilter.run
        (map (\freq ->
                UniFilter.parameter (FiltR.Pole 10 ((1800/sampleRate)*2**freq)))
             (Osci.staticSine phase (1/16/sampleRate))
        )


fatSawChordFilter, fatSawChord ::
   (RealField.C a, Trans.C a, Module.C a a) => a -> a -> [a]

fatSawChordFilter sampleRate freq =
    map (\r -> UniFilter.lowpass r / 2)
        (UniFilter.run (filterDown sampleRate)
                   (fatSawChord sampleRate freq))

fatSawChord sampleRate freq =
    zipWith3 (\x y z -> (x+y+z)/3)
             (fatSaw sampleRate (1  *freq))
             (fatSaw sampleRate (5/4*freq))
             (fatSaw sampleRate (3/2*freq))

filterDown :: (RealField.C a, Trans.C a) => a -> [UniFilter.Parameter a]

filterDown sampleRate =
    map UniFilter.parameter $
    map (FiltR.Pole 10) $
    exponential2 (sampleRate/3) (4000/sampleRate)

simpleSaw sampleRate freq = 
    Osci.staticSaw 0 (freq/sampleRate)

{-| accumulate multiple similar saw sounds and observe the increase of volume
    The oscillator @osc@ must accept relative frequencies. -}
modulatedWave :: (Trans.C a, RealField.C a) =>
   a -> (a -> [a] -> [a]) -> a -> a -> a -> a -> a -> [a]
modulatedWave sampleRate osc freq start depth phase speed =
   osc start (map (\x -> freq/sampleRate*(1+x*depth))
                  (Osci.staticSine phase (speed/sampleRate)))

accumulatedSaws :: (Random a, Trans.C a, RealField.C a) => a -> a -> [[a]]
accumulatedSaws sampleRate freq =
   let starts = randomRs (0,1)     (mkStdGen 48251)
       depths = randomRs (0,0.02)  (mkStdGen 12354)
       phases = randomRs (0,1)     (mkStdGen 74389)
       speeds = randomRs (0.1,0.3) (mkStdGen 03445)
       saws   = zipWith4 (modulatedWave sampleRate Osci.freqModSaw freq)
                         starts depths phases speeds
   in  scanl1 (zipWith (+)) saws

choirWave :: Field.C a => [a]
choirWave =
   [0.702727421560071, 0.7378359559947721, 0.7826845805704197, 0.6755514176072053,
   0.4513448069764686, 0.3272995923197175, 0.3404887595570093, 0.41416011004660863,
   0.44593673999775735, 0.4803528740412951, 0.48761174828621334, 0.44076701468836754,
   0.39642906530439503, 0.35467843549395706, 0.38054627445988315, 0.3888748481589558,
   0.35303993804564215, 0.3725196582177455, 0.44980257249714667, 0.5421204370443772,
   0.627630436752643, 0.6589491426946169, 0.619819155051891, 0.5821754728547365,
   0.5495877076869761, 0.5324446834830168, 0.47242861142812065, 0.3686685958119909,
   0.2781440436733245, 0.2582500464201269, 0.1955614176372372, 0.038373557320540604,
   -0.13132155046556182, -0.21867394831598339, -0.24302145520904606, -0.3096437514614372,
   -0.44774961666697943, -0.5889830267579028, -0.7168993833444837, -0.816723038671071,
   -0.8330283834679535, -0.8384077057999397, -0.8834813451725689, -0.9159391171556484,
   -0.9189751669797644, -0.8932026446626791, -0.8909164153221475, -0.9716732300637536,
   -1, -0.9253833606736654, -0.8568630538844477, -0.863932337623625,
   -0.857811827480001, -0.8131204084064676, -0.7839286071242304, -0.7036632045472225,
   -0.5824648346845637, -0.46123726085299827, -0.41391985851146285, -0.45323938111069567,
   -0.5336689022602625, -0.5831307769323063, -0.5693896103843189, -0.48596981886424745,
   -0.35791155598992863, -0.2661471984133689, -0.24158092840946802, -0.23965213828744264,
   -0.23421368394531547, -0.25130667896294306, -0.3116359503337366, -0.31263345635966144,
   -0.1879031874103659, -0.00020936838180399674, 0.18567090309156153, 0.2713525359068149,
   0.2979908042971701, 0.2957704726566382, 0.28820375086489286, 0.364513508557745,
   0.4520234711163569, 0.43210542988077005, 0.4064955825278379, 0.4416784798648095,
   0.5240917981530765, 0.6496469543088884, 0.7658103369723797, 0.8012776441058732,
   0.7824042138292476, 0.752678361663059, 0.760211176708886, 0.7308266231622353]


choir :: (Random a, Trans.C a, RealField.C a) => a -> a -> [a]
choir sampleRate freq =
   let starts = randomRs (0,1)     (mkStdGen 48251)
       depths = randomRs (0,0.02)  (mkStdGen 12354)
       phases = randomRs (0,1)     (mkStdGen 74389)
       speeds = randomRs (0.1,0.3) (mkStdGen 03445)
       voices = zipWith4 (modulatedWave sampleRate
                            (Osci.freqModSample Interpolation.constant choirWave) freq)
                         starts depths phases speeds
   in  map (*0.2) ((scanl1 (zipWith (+)) voices) !! 10)


fatSaw sampleRate freq =
    {- a simplified version of modulatedWave -}
    let partial depth modPhase modFreq =
           osciDoubleSaw sampleRate
              (map (\x -> freq*(1+x*depth))
                   (Osci.staticSine modPhase (modFreq/sampleRate)))
    in  zipWith3 (((((/3).).(+)).).(+))
            (partial 0.00311 0.0 20)
            (partial 0.00532 0.3 17)
            (partial 0.00981 0.9  6)

osciDoubleSaw :: (RealField.C a, Module.C a a) => a -> [a] -> [a]
osciDoubleSaw sampleRate =
    Osci.freqModSample Interpolation.linear [-1, -0.2, 0.5, -0.5, 0.2, 1.0] 0
      . map (/sampleRate)


{-| A tone with a waveform with roughly the dependency x -> x**p,
    where the waveform is normalized to constant quadratic norm -}
osciSharp :: (RealField.C a, Trans.C a) => a -> a -> [a]
osciSharp sampleRate freq =
   let --control = iterate (+ (-1/sampleRate)) 4
       control = exponential2 (0.01*sampleRate) 10
   in  Osci.shapeMod Wave.powerNormed2 0 (freq/sampleRate) control

{-| Build a saw sound from its harmonics and modulate it.
    Different to normal modulation
    I modulate each harmonic with the same depth rather than a proportional one. -}
osciAbsModSaw :: (RealField.C a, Trans.C a) => a -> a -> [a]
osciAbsModSaw sampleRate freq =
   let ratios     = map fromIntegral [(1::Int)..20]
       harmonic n = FiltNR.amplify (0.25/n)
          (Osci.freqModSine 0 (map (\x -> (n+0.03*x)*freq/sampleRate)
                              (Osci.staticSine 0 (1/sampleRate))))
   in  mixMulti (map harmonic ratios)

{-| Short pulsed Noise.white,
    i.e. Noise.white amplified with pulses of varying H\/L ratio. -}
pulsedNoise :: (Ring.C a, Random a, RealField.C a, Trans.C a) =>
       a
   ->  a   {-^ frequency of the pulses, interesting ones are around 100 Hz and below -}
   -> [a]
pulsedNoise sampleRate freq =
   zipWith3 (\thr0 thr1 x -> if thr0+1 < (thr1+1)*0.2 then x else 0)
            (Osci.staticSine 0 (freq/sampleRate)) (Osci.staticSine 0 (0.1/sampleRate)) Noise.white

noiseBass :: (Ring.C a, Random a, RealField.C a, Trans.C a, Module.C a a) =>
       a
   ->  a
   -> [a]
noiseBass sampleRate freq =
   let y  = FiltNR.envelope (exponential2 (0.1*sampleRate) 1) Noise.white
       ks = Comb.runProc (round (sampleRate/freq))
               (Filt1.lowpass
                   (repeat (Filt1.parameter (2000/sampleRate)))) y
   in  ks

{-| Drum sound using the Karplus-Strong-Algorithm
    This is a Noise.white enveloped by an exponential2
    which is piped through the Karplus-Strong machine
    for generating some frequency.
    The whole thing is then frequency modulated
    to give a falling frequency. -}
electroTom :: (Ring.C a, Random a, RealField.C a, Trans.C a, Module.C a a) =>
   a -> [a]
electroTom sampleRate =
   let y  = FiltNR.envelope (exponential2 (0.1*sampleRate) 1) Noise.white
       ks = Comb.runProc (round (sampleRate/30))
                     (Filt1.lowpass
                         (repeat $ Filt1.parameter (1000/sampleRate))) y
   in  Interpolation.multiRelativeZeroPadLinear 0 (exponential2 (0.3*sampleRate) 1) ks
