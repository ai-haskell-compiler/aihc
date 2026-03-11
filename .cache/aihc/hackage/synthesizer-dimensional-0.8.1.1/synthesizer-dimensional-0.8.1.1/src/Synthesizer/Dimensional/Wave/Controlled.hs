{-
ToDo:
Antialiasing waveforms and oscillators

I think the oscillators should always provide the frequency
to the apply method of a wave.
Then the waveform can decide whether it wants to use it or not.
We could make a type class for simple and bandlimited waveforms.
However, there is a fundamental problem:
Distortion of a waveform (wave shaping)
can turn bandlimited waveforms into ones without band limits.
-}
module Synthesizer.Dimensional.Wave.Controlled where

import Synthesizer.Dimensional.Wave (SamplePhase, )

import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Map as MapD

import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Generic.Wave as WaveG
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Interpolation as Interpolation

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealField      as RealField
import qualified Algebra.Ring           as Ring

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- |
We define a dimensional parametrized waveform in terms of a Map.
This allows any kind and number of control parameters
and distortion of waveforms using @(distortion <<<)@
-}
type T c t y = MapD.T (c, SamplePhase t) y


{-# INLINE simple #-}
simple ::
   (Amp.Primitive cAmp) =>
   amp ->
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.T amp y)
simple amp wave =
   MapD.independent
      (const $ amp)
      (\(c,p) -> Wave.apply (wave c) p)

{-# INLINE flat #-}
flat ::
   (Ring.C y, Amp.Primitive cAmp) =>
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.Flat y)
flat = simple Amp.Flat


{-# INLINE abstract #-}
abstract ::
   (Amp.Primitive cAmp) =>
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.Abstract y)
abstract = simple Amp.Abstract


{-# INLINE amplified #-}
amplified ::
   (Ring.C y, Dim.C u, Amp.Primitive cAmp) =>
   DN.T u y ->
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.Dimensional u y y)
amplified = simple . Amp.Numeric


{-# INLINE mapLinear #-}
mapLinear ::
   (Ring.C y, Dim.C u, Amp.Primitive cAmp) =>
   y ->
   DN.T u y ->
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.Dimensional u y y)
mapLinear depth center =
   amplified center . (Wave.distort (\x -> one+x*depth) .)

{-# INLINE mapExponential #-}
mapExponential ::
   (Trans.C y, Dim.C u, Amp.Primitive cAmp) =>
   y ->
   DN.T u y ->
   (c -> Wave.T t y) ->
   T (Sample.T cAmp c) t (Sample.Dimensional u y y)
mapExponential depth center =
   -- amplified center . Wave.distort (depth**)
   -- should be faster
   amplified center .
      let logDepth = log depth
      in  (Wave.distort (exp . (logDepth*)) .)



{- |
Interpolate first within waves and then across waves,
which is simpler but maybe less efficient for lists.
However for types with fast indexing/drop like StorableVector this is optimal.
-}
sampledTone ::
   (RealField.C t, SigG.Transform sig y, Dim.C u) =>
   Interpolation.T t y ->
   Interpolation.T t y ->
   DN.T u t ->
   SigA.T (Rate.Dimensional u t) amp (sig y) ->
   T (Sample.Flat t) t (Sample.T amp y)
sampledTone ipLeap ipStep period tone =
   simple
      (SigA.amplitude tone)
      (WaveG.sampledTone ipLeap ipStep
          (DN.mulToScalar period (SigA.actualSampleRate tone))
          (SigA.body tone))
