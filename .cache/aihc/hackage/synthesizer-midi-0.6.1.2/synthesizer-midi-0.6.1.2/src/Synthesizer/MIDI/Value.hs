{-# LANGUAGE NoImplicitPrelude #-}
{- |
Functions for converting MIDI controller and key values
to something meaningful for signal processing.
-}
module Synthesizer.MIDI.Value where

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field

import NumericPrelude.Numeric
-- import NumericPrelude.Base


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Field.C y) =>
   (y,y) -> Int -> y
controllerLinear (lower,upper) n =
   let k = fromIntegral n / 127
   in  (1-k) * lower + k * upper

{-# INLINE controllerExponential #-}
controllerExponential ::
   (Trans.C y) =>
   (y,y) -> Int -> y
controllerExponential (lower,upper) n =
   let k = fromIntegral n / 127
   in  lower**(1-k) * upper**k

{-# INLINE pitchBend #-}
pitchBend ::
   (Trans.C y) =>
   y -> y -> Int -> y
pitchBend range center n =
   center * range ** (fromIntegral n / 8192)

{-# INLINE velocity #-}
velocity ::
   (Field.C y) =>
   VoiceMsg.Velocity -> y
velocity vel =
   fromIntegral (VoiceMsg.fromVelocity vel - 64)/63

{- |
Convert pitch to frequency according to the default tuning
in MIDI 1.0 Detailed Specification.
-}
{-# INLINE frequencyFromPitch #-}
frequencyFromPitch ::
   (Trans.C y) =>
   VoiceMsg.Pitch -> y
frequencyFromPitch pitch =
   440 * 2 ^? (fromIntegral (VoiceMsg.fromPitch pitch + 3 - 6*12) / 12)
