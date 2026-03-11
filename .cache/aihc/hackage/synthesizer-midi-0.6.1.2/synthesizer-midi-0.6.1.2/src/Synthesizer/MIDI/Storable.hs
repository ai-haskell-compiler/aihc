{- |
Convert MIDI events of a MIDI controller to a control signal.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.Storable (
   chunkSizesFromLazyTime,
   piecewiseConstant,
   piecewiseConstantInit,
   piecewiseConstantInitWith,
   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   bendWheelPressure,

   Instrument, Bank,
   sequenceCore,
   sequence,
   sequenceModulated,
   sequenceMultiModulated,
   applyModulation,
   advanceModulationLazy,
   advanceModulationStrict,
   advanceModulationChunky,
   sequenceMultiProgram,

   Gen.renderInstrument,
   Gen.renderInstrumentIgnoreProgram,
   Gen.evaluateVectorHead,
   Gen.advanceModulationChunk,
   ) where

import Synthesizer.MIDI.EventList
          (LazyTime, StrictTime, Filter, Note,
           Program, Channel, Controller,
           getControllerEvents, getSlice, )
import qualified Synthesizer.MIDI.Generic as Gen
import qualified Synthesizer.MIDI.Value as MV

import qualified Synthesizer.Storable.Cut        as CutSt
import qualified Synthesizer.Storable.Signal     as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy        as SVL

import qualified Synthesizer.State.Signal       as SigS
import qualified Synthesizer.State.Oscillator   as OsciS
import qualified Synthesizer.State.Displacement as DispS
import qualified Synthesizer.State.Filter.NonRecursive as FiltNRS
import qualified Synthesizer.Basic.Wave         as Wave

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeBody  as EventList

import Foreign.Storable (Storable, )

import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Additive       as Additive

import Control.Monad.Trans.State (State, evalState, state, modify, put, get, )
import Control.Monad (liftM, )
import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )

import NumericPrelude.Base hiding (sequence, )
import NumericPrelude.Numeric



chunkSizesFromStrictTime :: StrictTime -> NonNegChunky.T SigSt.ChunkSize
chunkSizesFromStrictTime =
   NonNegChunky.fromChunks .
   map (SVL.ChunkSize . NonNegW.toNumber) .
   PC.chopLongTime


chunkSizesFromLazyTime :: LazyTime -> NonNegChunky.T SigSt.ChunkSize
chunkSizesFromLazyTime =
   NonNegChunky.fromChunks .
   map (SVL.ChunkSize . NonNegW.toNumber) .
   concatMap PC.chopLongTime .
   NonNegChunky.toChunks .
   NonNegChunky.normalize



{-
ToDo: move to Storable.Signal
-}
{-# INLINE piecewiseConstant #-}
piecewiseConstant ::
   (Storable y) =>
   EventListBT.T StrictTime y -> SigSt.T y
piecewiseConstant =
   EventListBT.foldrPair
      (\y t -> SigSt.append (SigStV.replicate (chunkSizesFromStrictTime t) y))
      SigSt.empty

{-# INLINE piecewiseConstantInit #-}
piecewiseConstantInit ::
   (Storable y) =>
   y -> EventList.T StrictTime y -> SigSt.T y
piecewiseConstantInit initial =
   (\ ~(t,rest) ->
      SigSt.append (SigStV.replicate (chunkSizesFromStrictTime t) initial) rest)
   .
   EventList.foldr
      (,)
      (\y ~(t,rest) ->
         SigSt.append (SigStV.replicate (chunkSizesFromStrictTime t) y) rest)
      (0, SigSt.empty)
{-
   piecewiseConstant .
--   EventListBM.switchBodyR const .
--   EventListBM.snocTime NonNeg.zero .
--   EventListMB.consBody initial .
   -- switchBodyR causes a space leak
   EventListTM.switchBodyR EventListBT.empty
      (\xs _ -> EventListMT.consBody initial xs)
-}

{-# INLINE piecewiseConstantInitWith #-}
piecewiseConstantInitWith ::
   (Storable c) =>
   (y -> c) ->
   c -> EventList.T StrictTime [y] -> SigSt.T c
piecewiseConstantInitWith f initial =
   piecewiseConstantInit initial .
   flip evalState initial .
   traverse (\evs -> traverse_ (put . f) evs >> get)


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Check.C event, Storable y, Field.C y) =>
   Channel -> Controller ->
   (y,y) -> y ->
   Filter event (SigSt.T y)
controllerLinear chan ctrl bnd initial =
   liftM (piecewiseConstantInitWith (MV.controllerLinear bnd) initial) $
   getControllerEvents chan ctrl


{-# INLINE controllerExponential #-}
controllerExponential ::
   (Check.C event, Storable y, Trans.C y) =>
   Channel -> Controller ->
   (y,y) -> y ->
   Filter event (SigSt.T y)
controllerExponential chan ctrl bnd initial =
   liftM (piecewiseConstantInitWith (MV.controllerExponential bnd) initial) $
   getControllerEvents chan ctrl


{- |
@pitchBend channel range center@:
emits frequencies on an exponential scale from
@center/range@ to @center*range@.
-}
{-# INLINE pitchBend #-}
pitchBend ::
   (Check.C event, Storable y, Trans.C y) =>
   Channel ->
   y -> y ->
   Filter event (SigSt.T y)
pitchBend chan range center =
   liftM (piecewiseConstantInitWith (MV.pitchBend range center) center) $
   getSlice (Check.pitchBend chan)
--   getPitchBendEvents chan

{-# INLINE channelPressure #-}
channelPressure ::
   (Check.C event, Storable y, Trans.C y) =>
   Channel ->
   y -> y ->
   Filter event (SigSt.T y)
channelPressure chan maxVal initVal =
   liftM (piecewiseConstantInitWith (MV.controllerLinear (0,maxVal)) initVal) $
   getSlice (Check.channelPressure chan)


{-
We could use 'getBendWheelPressureSignal' here,
but this may be less efficient.
-}
{-# INLINE bendWheelPressure #-}
bendWheelPressure ::
   (Check.C event, Storable y, RealRing.C y, Trans.C y) =>
   Channel ->
   Int -> y -> y -> y ->
   Filter event (SigSt.T y)
bendWheelPressure chan
     pitchRange speed wheelDepth pressDepth =
   do bend  <- pitchBend chan (2^?(fromIntegral pitchRange/12)) 1
      fm    <- controllerLinear chan VoiceMsg.modulation (0,wheelDepth) 0
      press <- channelPressure chan pressDepth 0
      return $
         flip (SigS.zipWithStorable (*)) bend $
         SigS.map (1+) $
         FiltNRS.envelope
            (DispS.mix
               (SigS.fromStorableSignal fm)
               (SigS.fromStorableSignal press))
            (OsciS.static Wave.sine zero speed)


type Instrument y yv = Gen.Instrument y (SigSt.T yv)
type Bank y yv = Gen.Bank y (SigSt.T yv)


{-# INLINE sequenceCore #-}
sequenceCore ::
   (Check.C event, Storable yv, Additive.C yv) =>
   SVL.ChunkSize ->
   Channel ->
   Program ->
   Gen.Modulator Note (SigSt.T yv) ->
   Filter event (SigSt.T yv)
sequenceCore chunkSize chan pgm modu =
   fmap (CutSt.arrangeEquidist chunkSize) $
   Gen.sequenceCore chan pgm modu


{-# INLINE sequence #-}
sequence ::
   (Check.C event, Storable yv, Additive.C yv, Trans.C y) =>
   SVL.ChunkSize ->
   Channel ->
   Instrument y yv ->
   Filter event (SigSt.T yv)
sequence chunkSize chan bank =
   fmap (CutSt.arrangeEquidist chunkSize) $
   Gen.sequence chan bank


{-# INLINE sequenceModulated #-}
sequenceModulated ::
   (Check.C event, Storable c, Storable yv, Additive.C yv, Trans.C y) =>
   SVL.ChunkSize ->
   SigSt.T c ->
   Channel ->
   (SigSt.T c -> Instrument y yv) ->
   Filter event (SigSt.T yv)
sequenceModulated chunkSize modu chan instr =
   fmap (CutSt.arrangeEquidist chunkSize) $
   Gen.sequenceModulated modu chan instr


{-# INLINE sequenceMultiModulated #-}
sequenceMultiModulated ::
   (Check.C event, Storable yv, Additive.C yv, Trans.C y) =>
   SVL.ChunkSize ->
   Channel ->
   instrument ->
   Gen.Modulator (instrument, Note) (Instrument y yv, Note) ->
   Filter event (SigSt.T yv)
sequenceMultiModulated chunkSize chan instr modu =
   fmap (CutSt.arrangeEquidist chunkSize) $
   Gen.sequenceMultiModulated chan instr modu


applyModulation ::
   (Storable c) =>
   SigSt.T c ->
   Gen.Modulator (SigSt.T c -> instr, note) (instr, note)
applyModulation =
   Gen.applyModulation

advanceModulationLazy, advanceModulationStrict, advanceModulationChunky ::
   (Storable a) =>
   LazyTime -> State (SigSt.T a) LazyTime

{-
This one drops lazily,
such that the control signal will be cached until it is used.
That is, if for a long time no new note is played,
more and more memory will be allocated.
-}
advanceModulationLazy t =
   modify (SigStV.drop (chunkSizesFromLazyTime t)) >> return t

{-
This one is too strict,
because the complete drop is forced
also if only the first chunk of the lazy time is requested.
-}
advanceModulationStrict t = state $ \xs ->
   let ys = SigStV.drop (chunkSizesFromLazyTime t) xs
   in  (Gen.evaluateVectorHead ys t, ys)

advanceModulationChunky =
   Gen.advanceModulation


{-# INLINE sequenceMultiProgram #-}
sequenceMultiProgram ::
   (Check.C event, Storable yv, Additive.C yv, Trans.C y) =>
   SVL.ChunkSize ->
   Channel ->
   Program ->
   [Instrument y yv] ->
   Filter event (SigSt.T yv)
sequenceMultiProgram chunkSize chan pgm bank =
   fmap (CutSt.arrangeEquidist chunkSize) $
   Gen.sequenceMultiProgram chan pgm bank
