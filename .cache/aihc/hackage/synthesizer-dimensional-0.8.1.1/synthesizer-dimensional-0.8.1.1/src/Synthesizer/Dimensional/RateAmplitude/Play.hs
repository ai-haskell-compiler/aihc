{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.Dimensional.RateAmplitude.Play (
   auto,
   timeVoltage,
   timeVoltageMonoDoubleToInt16,
   timeVoltageStereoDoubleToInt16,
   renderTimeVoltage,
   renderTimeVoltageMonoDoubleToInt16,
   renderTimeVoltageStereoDoubleToInt16,
  ) where

import qualified Sound.Sox.Play as Play
import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Sound.Sox.Frame as Frame
import qualified Synthesizer.Basic.Binary as BinSmp
import qualified Data.StorableVector.Lazy.Builder as Builder
import Foreign.Storable (Storable, )

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import qualified Algebra.ToInteger      as ToInteger
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field

import System.Exit(ExitCode)

import NumericPrelude.Numeric
import NumericPrelude.Base


type Signal u t v y yv =
   SigA.T (Rate.Dimensional u t) (Amp.Dimensional v y) (Sig.T yv)


{-# INLINE auto #-}
auto ::
    (Bounded int, ToInteger.C int, Storable int, Frame.C int,
     Dim.C u, RealField.C t,
     Dim.C v, BinSmp.C yv, Module.C y yv, Field.C y) =>
   DN.T (Dim.Recip u) t ->
   DN.T v y ->
   (int -> Builder.Builder int) ->
   Signal u t v y yv ->
   IO ExitCode
auto freqUnit amp put sig =
   let opts =
          SoxOpt.numberOfChannels $
          BinSmp.numberOfSignalChannels $
          SigA.body sig
       sampleRate =
          DN.divToScalar (SigA.actualSampleRate sig) freqUnit
   in  Play.extended SigSt.hPut opts SoxOpt.none
          (round sampleRate)
          (Builder.toLazyStorableVector SigA.defaultChunkSize $
           Sig.foldMap (BinSmp.outputFromCanonical put) $
           SigA.vectorSamples (flip DN.divToScalar amp) sig)


{-# INLINE timeVoltage #-}
timeVoltage ::
    (Bounded int, ToInteger.C int, Storable int, Frame.C int,
     RealField.C t,
     BinSmp.C yv, Module.C y yv, Field.C y) =>
   (int -> Builder.Builder int) ->
   Signal Dim.Time t Dim.Voltage y yv ->
   IO ExitCode
timeVoltage =
   auto (DN.frequency one) (DN.voltage one)


{-# INLINE timeVoltageMonoDoubleToInt16 #-}
timeVoltageMonoDoubleToInt16 ::
   Signal Dim.Time Double Dim.Voltage Double Double ->
   IO ExitCode
timeVoltageMonoDoubleToInt16 sig =
   let rate = DN.toNumberWithDimension Dim.frequency (SigA.actualSampleRate sig)
   in  Play.simple SigSt.hPut SoxOpt.none (round rate)
          (SigA.toStorableInt16Mono sig)


{-# INLINE timeVoltageStereoDoubleToInt16 #-}
timeVoltageStereoDoubleToInt16 ::
   Signal Dim.Time Double Dim.Voltage Double (Stereo.T Double) ->
   IO ExitCode
timeVoltageStereoDoubleToInt16 sig =
   let rate = DN.toNumberWithDimension Dim.frequency (SigA.actualSampleRate sig)
   in  Play.simple SigSt.hPut SoxOpt.none (round rate)
          (SigA.toStorableInt16Stereo sig)


{-# INLINE renderTimeVoltage #-}
renderTimeVoltage ::
    (Bounded int, ToInteger.C int, Storable int, Frame.C int,
     RealField.C t,
     BinSmp.C yv, Module.C y yv, Field.C y) =>
   (int -> Builder.Builder int) ->
   DN.T Dim.Frequency t ->
   (forall s. Proc.T s Dim.Time t (SigA.R s Dim.Voltage y yv)) ->
   IO ExitCode
renderTimeVoltage put rate sig =
   timeVoltage put (SigA.render rate sig)

{-# INLINE renderTimeVoltageMonoDoubleToInt16 #-}
renderTimeVoltageMonoDoubleToInt16 ::
   DN.T Dim.Frequency Double ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)) ->
   IO ExitCode
renderTimeVoltageMonoDoubleToInt16 rate sig =
   timeVoltageMonoDoubleToInt16 (SigA.render rate sig)

{-# INLINE renderTimeVoltageStereoDoubleToInt16 #-}
renderTimeVoltageStereoDoubleToInt16 ::
   DN.T Dim.Frequency Double ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))) ->
   IO ExitCode
renderTimeVoltageStereoDoubleToInt16 rate sig =
   timeVoltageStereoDoubleToInt16 (SigA.render rate sig)
