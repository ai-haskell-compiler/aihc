{-# LANGUAGE NoImplicitPrelude #-}
{- |
Functions for converting MIDI controller and key values
to something meaningful for signal processing.
-}
module Synthesizer.MIDI.Dimensional.Value (
   controllerLinear,
   controllerExponential,
   pitchBend,
   MV.frequencyFromPitch,
   ) where

import qualified Synthesizer.MIDI.Dimensional.ValuePlain as MV

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm as DN

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
-- import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric
-- import NumericPrelude.Base


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Field.C y, Dim.C v) =>
   DN.T v y -> (DN.T v y, DN.T v y) -> Int -> y
controllerLinear amp bnd n =
   DN.divToScalar (MV.controllerLinear bnd n) amp

{-# INLINE controllerExponential #-}
controllerExponential ::
   (Trans.C y, Dim.C v) =>
   DN.T v y -> (DN.T v y, DN.T v y) -> Int -> y
controllerExponential amp bnd n =
   DN.divToScalar (MV.controllerExponential bnd n) amp

{-# INLINE pitchBend #-}
pitchBend ::
   (Trans.C y, Dim.C v) =>
   DN.T v y -> y -> DN.T v y -> Int -> y
pitchBend amp range center n =
   DN.divToScalar (MV.pitchBend range center n) amp
