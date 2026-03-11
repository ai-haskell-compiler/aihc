{-# LANGUAGE NoImplicitPrelude #-}
{- |
Functions for converting MIDI controller and key values
to something meaningful for signal processing.
-}
module Synthesizer.MIDI.Dimensional.ValuePlain (
   controllerLinear,
   controllerExponential,
   pitchBend,
   frequencyFromPitch,
   ) where

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm as DN

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
-- import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Field.C y, Dim.C v) =>
   (DN.T v y, DN.T v y) -> Int -> DN.T v y
controllerLinear (lower,upper) n =
   let k = fromIntegral n / 127
   in  DN.scale (1-k) lower + DN.scale k upper

{-# INLINE controllerExponential #-}
controllerExponential ::
   (Trans.C y, Dim.C v) =>
   (DN.T v y, DN.T v y) -> Int -> DN.T v y
controllerExponential (lower,upper) n =
   let k = fromIntegral n / 127
   in  case error "MIDIValue.controllerExponential dimension" of
          d ->
             DN.fromNumberWithDimension d $
             DN.toNumberWithDimension d lower ** (1-k) *
             DN.toNumberWithDimension d upper ** k

{-# INLINE pitchBend #-}
pitchBend ::
   (Trans.C y, Dim.C v) =>
   y -> DN.T v y -> Int -> DN.T v y
pitchBend range center n =
   DN.scale (range ** (fromIntegral n / 8192)) center

{- |
Convert pitch to frequency according to the default tuning
in MIDI 1.0 Detailed Specification.
-}
{-# INLINE frequencyFromPitch #-}
frequencyFromPitch ::
   (Trans.C y) =>
   VoiceMsg.Pitch -> DN.Frequency y
frequencyFromPitch pitch =
   DN.scale
      (2 ^? (fromIntegral (VoiceMsg.fromPitch pitch + 3 - 6*12) / 12))
      (DN.frequency 440)
