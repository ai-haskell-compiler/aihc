module Synthesizer.Dimensional.Wave where

import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Map as MapD

import qualified Synthesizer.Basic.Phase as Phase
import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Generic.Wave as WaveG
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Interpolation as Interpolation

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Ring           as Ring

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


type SamplePhase t = Sample.Abstract (Phase.T t)

{- |
We define a dimensional waveform in terms of a Map.
This allows any kind and number of result samples
and distortion of waveforms using @(distortion <<<)@
-}
type T t y = MapD.T (SamplePhase t) y

{-# INLINE simple #-}
simple ::
   amp ->
   Wave.T t y ->
   T t (Sample.T amp y)
simple amp wave =
   MapD.independent
      (const $ amp)
      (Wave.apply wave)


infix 7 &*~

{-# INLINE (&*~) #-}
(&*~) ::
   amp ->
   Wave.T t y ->
   T t (Sample.Numeric amp y)
(&*~) = amplified


{-# INLINE sample #-}
sample ::
   (RealRing.C t, SigG.Transform sig y) =>
   Interpolation.T t y ->
   SigA.T rate amp (sig y) ->
   T t (Sample.T amp y)
sample ip wave =
   simple (SigA.amplitude wave) $
   WaveG.sample ip (SigA.body wave)


{-# INLINE flat #-}
flat :: (Ring.C y) =>
   Wave.T t y ->
   T t (Sample.Flat y)
flat = simple Amp.Flat


{-# INLINE abstract #-}
abstract ::
   Wave.T t y ->
   T t (Sample.Abstract y)
abstract = simple Amp.Abstract


{-# INLINE amplified #-}
amplified ::
   amp ->
   Wave.T t y ->
   T t (Sample.Numeric amp y)
{-
 (Ring.C y, Dim.C u) =>
   DN.T u y ->
   Wave.T t y ->
   T t (Sample.Dimensional u y y)
-}
{-
   amp ->
   Wave.T t y ->
   T amp t y
-}
amplified = simple . Amp.Numeric


{-# INLINE mapLinear #-}
mapLinear :: (Ring.C y, Dim.C u) =>
   y ->
   DN.T u y ->
   Wave.T t y ->
   T t (Sample.Dimensional u y y)
mapLinear depth center =
   amplified center . Wave.distort (\x -> one+x*depth)

{-# INLINE mapExponential #-}
mapExponential :: (Trans.C y, Dim.C u) =>
   y ->
   DN.T u y ->
   Wave.T t y ->
   T t (Sample.Dimensional u y y)
mapExponential depth center =
   -- amplified center . Wave.distort (depth**)
   -- should be faster
   amplified center .
      let logDepth = log depth
      in  Wave.distort (exp . (logDepth*))
