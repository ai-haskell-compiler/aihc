{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.ALSA.Dimensional.Play where

import qualified Synthesizer.ALSA.Storable.Play as Play

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Synthesizer.Storable.Signal as SigSt

import qualified Sound.ALSA.PCM as ALSA

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

-- import qualified Algebra.ToInteger      as ToInteger
import qualified Algebra.Module         as Module
import qualified Algebra.RealRing      as RealRing
-- import qualified Algebra.Field          as Field
-- import qualified Algebra.Ring           as Ring

import Foreign.Storable (Storable, )

-- import NumericPrelude.Numeric
import NumericPrelude.Base


type Device = String

type RenderedStorableSignal u t v y yv =
   SigA.T (Rate.Dimensional u t) (Amp.Dimensional v y) (SigSt.T yv)

type StorableSignal s v y yv =
   SigA.T (Rate.Phantom s) (Amp.Dimensional v y) (SigSt.T yv)


makeSink ::
   (ALSA.SampleFmt y, RealRing.C t) =>
   Device {- ^ ALSA output device -} ->
   DN.Time t {- ^ period (buffer) size expressed in seconds -} ->
   DN.Frequency t {- ^ sample rate -} ->
   ALSA.SoundSink ALSA.Pcm y
makeSink device periodTime rate =
   Play.makeSink device
      (DN.toNumberWithDimension Dim.time periodTime)
      (RealRing.round (DN.toNumberWithDimension Dim.frequency rate))


{-# INLINE timeVoltageStorable #-}
timeVoltageStorable ::
   (Module.C y yv, ALSA.SampleFmt yv, RealRing.C t) =>
   Device ->
   DN.Time t->
   RenderedStorableSignal Dim.Time t Dim.Voltage y yv ->
   IO ()
timeVoltageStorable device period sig =
   Play.auto (makeSink device period (SigA.actualSampleRate sig))
      (SigA.vectorSamples (DN.toNumberWithDimension Dim.voltage) sig)

{-# INLINE timeVoltageMonoStorableToInt16 #-}
timeVoltageMonoStorableToInt16 ::
   (Storable y, RealRing.C y, RealRing.C t) =>
   Device ->
   DN.Time t->
   RenderedStorableSignal Dim.Time t Dim.Voltage y y ->
   IO ()
timeVoltageMonoStorableToInt16 device period sig =
   Play.monoToInt16 (makeSink device period (SigA.actualSampleRate sig))
      (SigA.scalarSamples (DN.toNumberWithDimension Dim.voltage) sig)

{-# INLINE timeVoltageStereoStorableToInt16 #-}
timeVoltageStereoStorableToInt16 ::
   (Storable y, Module.C y y, RealRing.C y, RealRing.C t) =>
   Device ->
   DN.Time t->
   RenderedStorableSignal Dim.Time t Dim.Voltage y (Stereo.T y) ->
   IO ()
timeVoltageStereoStorableToInt16 device period sig =
   Play.stereoToInt16 (makeSink device period (SigA.actualSampleRate sig))
      (SigA.vectorSamples (DN.toNumberWithDimension Dim.voltage) sig)


{-# INLINE renderTimeVoltageStorable #-}
renderTimeVoltageStorable ::
   (Module.C y yv, ALSA.SampleFmt yv, RealRing.C t) =>
   Device ->
   DN.Time t->
   DN.T Dim.Frequency t ->
   (forall s. Proc.T s Dim.Time t
      (StorableSignal s Dim.Voltage y yv)) ->
   IO ()
renderTimeVoltageStorable device period rate sig =
   timeVoltageStorable device period (SigA.render rate sig)

{-# INLINE renderTimeVoltageMonoStorableToInt16 #-}
renderTimeVoltageMonoStorableToInt16 ::
   (Storable y, RealRing.C y, RealRing.C t) =>
   Device ->
   DN.Time t->
   DN.T Dim.Frequency t ->
   (forall s. Proc.T s Dim.Time t
      (StorableSignal s Dim.Voltage y y)) ->
   IO ()
renderTimeVoltageMonoStorableToInt16 device period rate sig =
   timeVoltageMonoStorableToInt16 device period (SigA.render rate sig)

{-# INLINE renderTimeVoltageStereoStorableToInt16 #-}
renderTimeVoltageStereoStorableToInt16 ::
   (Storable y, Module.C y y, RealRing.C y, RealRing.C t) =>
   Device ->
   DN.Time t->
   DN.T Dim.Frequency t ->
   (forall s. Proc.T s Dim.Time t
      (StorableSignal s Dim.Voltage y (Stereo.T y))) ->
   IO ()
renderTimeVoltageStereoStorableToInt16 device period rate sig =
   timeVoltageStereoStorableToInt16 device period (SigA.render rate sig)
