{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Tone generators
-}
module Synthesizer.Storable.Oscillator where

import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Basic.Phase      as Phase

import qualified Synthesizer.Storable.Signal as Signal
import Synthesizer.Storable.Signal (ChunkSize)
import Foreign.Storable (Storable)

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealRing              as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base



{- * Oscillators with arbitrary but constant waveforms -}

{-# INLINE freqToPhase #-}
{- | Convert a list of phase steps into a list of momentum phases
     phase is a number in the interval [0,1)
     freq contains the phase steps -}
freqToPhase :: (RealRing.C a, Storable a) =>
   Phase.T a -> Signal.T a -> Signal.T (Phase.T a)
freqToPhase phase freq = Signal.scanL (flip Phase.increment) phase freq


{-# INLINE static #-}
{- disabled SPECIALISE static :: Storable b => ChunkSize -> (Double -> b) -> (Double -> Double -> Signal.T b) -}
{- | oscillator with constant frequency -}
static :: (RealRing.C a, Storable a, Storable b) =>
    ChunkSize -> Wave.T a b -> (Phase.T a -> a -> Signal.T b)
static size wave phase freq =
    Signal.map (Wave.apply wave) (Signal.iterate size (Phase.increment freq) phase)

{- | oscillator with modulated phase -}
phaseMod :: (RealRing.C a, Storable a, Storable b) =>
    ChunkSize -> Wave.T a b -> a -> Signal.T a -> Signal.T b
phaseMod size wave = shapeMod size (Wave.phaseOffset wave) zero

{- disabled INLINE shapeMod -}
{- | oscillator with modulated shape -}
shapeMod :: (RealRing.C a, Storable a, Storable b, Storable c) =>
    ChunkSize -> (c -> Wave.T a b) -> Phase.T a -> a -> Signal.T c -> Signal.T b
shapeMod size wave phase freq parameters =
    Signal.zipWith (Wave.apply . wave) parameters
       (Signal.iterate size (Phase.increment freq) phase)

{- | oscillator with modulated frequency -}
freqMod :: (RealRing.C a, Storable a, Storable b) =>
    ChunkSize -> Wave.T a b -> Phase.T a -> Signal.T a -> Signal.T b
freqMod _size wave phase freqs =
    Signal.map (Wave.apply wave) (freqToPhase phase freqs)

{- | oscillator with both phase and frequency modulation -}
phaseFreqMod :: (RealRing.C a, Storable a, Storable b) =>
    ChunkSize -> Wave.T a b -> Signal.T a -> Signal.T a -> Signal.T b
phaseFreqMod size wave =
    shapeFreqMod size (Wave.phaseOffset wave) zero

{- | oscillator with both shape and frequency modulation -}
shapeFreqMod :: (RealRing.C a, Storable a, Storable b, Storable c) =>
    ChunkSize -> (c -> Wave.T a b) ->
    Phase.T a -> Signal.T c -> Signal.T a -> Signal.T b
shapeFreqMod _size wave phase parameters freqs =
    Signal.zipWith (Wave.apply . wave) parameters (freqToPhase phase freqs)


{-
{- | oscillator with a sampled waveform with constant frequency
     This essentially an interpolation with cyclic padding. -}
staticSample :: RealRing.C a => Interpolation.T a b -> Signal.T b -> a -> a -> Signal.T b
staticSample ip wave phase freq =
    freqModSample ip wave phase (repeat freq)

{- | oscillator with a sampled waveform with modulated frequency
     Should behave homogenously for different types of interpolation. -}
freqModSample :: RealRing.C a => Interpolation.T a b -> Signal.T b -> a -> Signal.T a -> Signal.T b
freqModSample ip wave phase freqs =
    let len = fromIntegral (length wave)
    in  Interpolation.multiRelativeCyclicPad
           ip (phase*len) (Signal.map (*len) freqs) wave
-}



{- * Oscillators with specific waveforms -}

{-# INLINE staticSine #-}
{- disabled SPECIALISE staticSine :: ChunkSize -> Double -> Double -> Signal.T Double -}
{- | sine oscillator with static frequency -}
staticSine :: (Trans.C a, RealRing.C a, Storable a) =>
   ChunkSize -> Phase.T a -> a -> Signal.T a
staticSine size = static size Wave.sine

{-# INLINE freqModSine #-}
{- disabled SPECIALISE freqModSine :: ChunkSize -> Double -> Signal.T Double -> Signal.T Double -}
{- | sine oscillator with modulated frequency -}
freqModSine :: (Trans.C a, RealRing.C a, Storable a) =>
   ChunkSize -> Phase.T a -> Signal.T a -> Signal.T a
freqModSine size = freqMod size Wave.sine

{-# INLINE phaseModSine #-}
{- disabled SPECIALISE phaseModSine :: ChunkSize -> Double -> Signal.T Double -> Signal.T Double -}
{- | sine oscillator with modulated phase, useful for FM synthesis -}
phaseModSine :: (Trans.C a, RealRing.C a, Storable a) =>
   ChunkSize -> a -> Signal.T a -> Signal.T a
phaseModSine size = phaseMod size Wave.sine

{-# INLINE staticSaw #-}
{- disabled SPECIALISE staticSaw :: ChunkSize -> Double -> Double -> Signal.T Double -}
{- | saw tooth oscillator with modulated frequency -}
staticSaw :: (RealRing.C a, Storable a) =>
   ChunkSize -> Phase.T a -> a -> Signal.T a
staticSaw size = static size Wave.saw

{-# INLINE freqModSaw #-}
{- disabled SPECIALISE freqModSaw :: ChunkSize -> Double -> Signal.T Double -> Signal.T Double -}
{- | saw tooth oscillator with modulated frequency -}
freqModSaw :: (RealRing.C a, Storable a) =>
   ChunkSize -> Phase.T a -> Signal.T a -> Signal.T a
freqModSaw size = freqMod size Wave.saw


{- Test whether Fusion takes place.
For the following code the simplifier can't resist!

testLength :: (Storable a, Enum a) => a -> Int
testLength x =
   Signal.length (Signal.map succ (Signal.fromList (Signal.ChunkSize 100) [x,x,x]))
-}
