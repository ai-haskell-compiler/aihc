{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.Dimensional.RateAmplitude.File (
   write,
   writeTimeVoltage,
   writeTimeVoltageMonoDoubleToInt16,
   writeTimeVoltageStereoDoubleToInt16,
   renderTimeVoltageMonoDoubleToInt16,
   renderTimeVoltageStereoDoubleToInt16,
  ) where

import qualified Sound.Sox.Write as Write
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

import qualified Algebra.ToInteger      as ToInteger
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN


import System.Exit(ExitCode)

import NumericPrelude.Numeric
import NumericPrelude.Base



type Signal u t v y yv =
   SigA.T (Rate.Dimensional u t) (Amp.Dimensional v y) (Sig.T yv)

{- |
The output format is determined by SoX by the file name extension.
The sample precision is determined by the provided 'Builder.Builder' function.

Example:

> import qualified Data.StorableVector.Lazy.Builder as Builder
>
> write (DN.frequency one) (DN.voltage one) (\i -> Builder.put (i::Int16)) "test.aiff" sound
-}
{-# INLINE write #-}
write ::
    (Bounded int, ToInteger.C int, Storable int, Frame.C int, BinSmp.C yv,
     Dim.C u, RealField.C t,
     Dim.C v, Module.C y yv, Field.C y) =>
   DN.T (Dim.Recip u) t ->
   DN.T v y ->
   (int -> Builder.Builder int) ->
   FilePath ->
   Signal u t v y yv ->
   IO ExitCode
write freqUnit amp put name sig =
   let opts =
          SoxOpt.numberOfChannels $
          BinSmp.numberOfSignalChannels $
          SigA.body sig
       sampleRate =
          DN.divToScalar (SigA.actualSampleRate sig) freqUnit
   in  Write.extended SigSt.hPut opts SoxOpt.none name
          (round sampleRate)
          (Builder.toLazyStorableVector SigSt.defaultChunkSize $
           Sig.foldMap (BinSmp.outputFromCanonical put) $
           -- ToDo: flip DN.divToScalar -> ampToScalar
           SigA.vectorSamples (flip DN.divToScalar amp) sig)


{-# INLINE writeTimeVoltage #-}
writeTimeVoltage ::
    (Bounded int, ToInteger.C int, Storable int, Frame.C int, BinSmp.C yv,
     RealField.C t,
     Module.C y yv, Field.C y) =>
   (int -> Builder.Builder int) ->
   FilePath ->
   Signal Dim.Time t Dim.Voltage y yv ->
   IO ExitCode
writeTimeVoltage =
   write (DN.frequency one) (DN.voltage one)



{-# INLINE writeTimeVoltageMonoDoubleToInt16 #-}
writeTimeVoltageMonoDoubleToInt16 ::
   FilePath ->
   Signal Dim.Time Double Dim.Voltage Double Double ->
   IO ExitCode
writeTimeVoltageMonoDoubleToInt16 name sig =
   let rate = DN.toNumberWithDimension Dim.frequency (SigA.actualSampleRate sig)
   in  Write.simple SigSt.hPut SoxOpt.none name (round rate)
          (SigA.toStorableInt16Mono sig)


{-# INLINE writeTimeVoltageStereoDoubleToInt16 #-}
writeTimeVoltageStereoDoubleToInt16 ::
   FilePath ->
   Signal Dim.Time Double Dim.Voltage Double (Stereo.T Double) ->
   IO ExitCode
writeTimeVoltageStereoDoubleToInt16 name sig =
   let rate = DN.toNumberWithDimension Dim.frequency (SigA.actualSampleRate sig)
   in  Write.simple SigSt.hPut SoxOpt.none name (round rate)
          (SigA.toStorableInt16Stereo sig)

{-# INLINE renderTimeVoltageMonoDoubleToInt16 #-}
renderTimeVoltageMonoDoubleToInt16 ::
   DN.T Dim.Frequency Double ->
   FilePath ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double Double)) ->
   IO ExitCode
renderTimeVoltageMonoDoubleToInt16 rate name sig =
   writeTimeVoltageMonoDoubleToInt16 name (SigA.render rate sig)

{-# INLINE renderTimeVoltageStereoDoubleToInt16 #-}
renderTimeVoltageStereoDoubleToInt16 ::
   DN.T Dim.Frequency Double ->
   FilePath ->
   (forall s. Proc.T s Dim.Time Double (SigA.R s Dim.Voltage Double (Stereo.T Double))) ->
   IO ExitCode
renderTimeVoltageStereoDoubleToInt16 rate name sig =
   writeTimeVoltageStereoDoubleToInt16 name (SigA.render rate sig)
