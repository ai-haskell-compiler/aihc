{-# LANGUAGE ExistentialQuantification #-}
{- |
Convert MIDI events of a MIDI controller to a control signal.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.Generic where

import Synthesizer.MIDI.EventList
   (LazyTime, StrictTime, Filter, Channel,
    Program, embedPrograms, makeInstrumentArray, getInstrumentFromArray,
    Note(Note), matchNoteEvents, getNoteEvents, )

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel as ChannelMsg

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.Generic.Cut        as CutG
import qualified Synthesizer.Generic.Signal     as SigG

import qualified Synthesizer.MIDI.Value as MV

import qualified Data.EventList.Relative.MixedBody as EventListMB
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeBody  as EventList

import Data.Monoid (Monoid, mempty, )

import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import qualified Algebra.Transcendental as Trans

import Control.Arrow (Arrow, arr, first, )
import Control.Category (Category, id, (.), )
import qualified Control.Monad.Trans.State.Strict as MS
import Control.Monad.Trans.State
          (State, evalState, runState, state, gets, put, get, )
import Control.Monad (liftM, )
import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )

import Control.DeepSeq (NFData, )

import NumericPrelude.Base hiding (id, (.), )
import NumericPrelude.Numeric
import Prelude ()



{-
ToDo: move to Generic.Signal
-}
replicateLong ::
   (SigG.Write sig y) =>
   StrictTime -> y -> sig y
replicateLong tl y =
   CutG.concat $
   map (\t ->
      SigG.replicate
--         (SigG.LazySize $ fromIntegral $ maxBound::Int)
         SigG.defaultLazySize
         (NonNegW.toNumber t) y) $
   PC.chopLongTime tl

{-
ToDo: move to Generic.Signal
-}
{-# INLINE piecewiseConstant #-}
piecewiseConstant ::
   (SigG.Write sig y) =>
   EventListBT.T StrictTime y -> sig y
piecewiseConstant =
   EventListBT.foldrPair
      (\y t -> SigG.append (replicateLong t y))
      SigG.empty

{-# INLINE piecewiseConstantInit #-}
piecewiseConstantInit ::
   (SigG.Write sig y) =>
   y -> EventList.T StrictTime y -> sig y
piecewiseConstantInit initial =
   (\ ~(t,rest) ->
      SigG.append (replicateLong t initial) rest)
   .
   EventList.foldr
      (,)
      (\y ~(t,rest) ->
         SigG.append (replicateLong t y) rest)
      (0, SigG.empty)

{-# INLINE piecewiseConstantInitWith #-}
piecewiseConstantInitWith ::
   (SigG.Write sig c) =>
   (y -> c) ->
   c -> EventList.T StrictTime [y] -> sig c
piecewiseConstantInitWith f initial =
   piecewiseConstantInit initial .
   flip evalState initial .
   traverse (\evs -> traverse_ (put . f) evs >> get)



type Instrument y signal = y -> y -> LazyTime -> signal
type Bank y signal = Program -> Instrument y signal

{- |
Instrument parameters are:
velocity from -1 to 1 (0 is the normal pressure, no pressure aka NoteOff is not supported),
frequency is given in Hertz
-}
renderInstrument ::
   (Trans.C y) =>
   Bank y signal ->
   Note ->
   signal
renderInstrument instrument (Note pgm pitch vel dur) =
   instrument pgm
      (MV.velocity vel)
      (MV.frequencyFromPitch pitch)
      dur

renderInstrumentIgnoreProgram ::
   (Trans.C y) =>
   Instrument y signal ->
   Note ->
   signal
renderInstrumentIgnoreProgram instrument =
   renderInstrument (const instrument)


{- |
Turn an event list with bundles of elements
into an event list with single events.
ToDo: Move to event-list package?
-}
flatten ::
   (Monoid signal, NonNeg.C time) =>
   EventList.T time [signal] ->
   EventList.T time signal
flatten =
   EventList.foldr
      EventListMB.consTime
      (\bt xs ->
         uncurry EventListMB.consBody $
         case bt of
            [] -> (mempty, xs)
            b:bs -> (b, foldr (EventList.cons NonNeg.zero) xs bs))
      EventList.empty


applyModulation ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   signal ->
   Modulator (signal -> instr, note) (instr, note)
applyModulation ctrl =
   first $
   Modulator ctrl advanceModulationChunk gets

{- |
We have to evaluate the head value at each 'drop'
in order to avoid growing thunks that lead to a space leak.
-}
evaluateVectorHead ::
   (CutG.NormalForm signal) =>
   signal -> t -> t
evaluateVectorHead xs t =
   case CutG.evaluateHead xs of () -> t
--   if CutG.null xs then t else t

advanceModulation ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   LazyTime -> State signal LazyTime
advanceModulation =
   liftM NonNegChunky.fromChunks .
   mapM advanceModulationChunk .
   NonNegChunky.toChunks

advanceModulationChunk ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   StrictTime -> State signal StrictTime
advanceModulationChunk t = state $ \xs ->
   let ys = CutG.drop (fromIntegral t) xs
   in  (evaluateVectorHead ys t, ys)

advanceModulationChunkStrict ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   StrictTime -> MS.State signal StrictTime
advanceModulationChunkStrict t = MS.state $ \xs ->
   let ys = CutG.drop (fromIntegral t) xs
   in  (evaluateVectorHead ys t, ys)

advanceModulationChunkPC ::
   (NFData body) =>
   StrictTime ->
   State (EventListBT.T StrictTime body) StrictTime
advanceModulationChunkPC t = state $ \xs ->
   let ys =
          EventListBT.fromPairList $ tail $
          EventListBT.toPairList xs
   in  (evaluateVectorHead ys t, ys)

type FilterSequence event signal =
   Filter event (EventList.T PC.ShortStrictTime signal)

{- |
The state action for the time
should just return the argument time.
However we need this time (or alternatively another result type)
for triggering the 'drop' in 'advanceModulationChunk'.
Without this strict evaluation,
the drop will be delayed until the control curve is actually needed.
-}
data Modulator note signal =
   forall state.
   Modulator
      state
      (StrictTime -> State state StrictTime)
      (note -> State state signal)

instance Category Modulator where
   id = Modulator () return return
   (Modulator yInit yTime yBody) . (Modulator xInit xTime xBody) =
      let compose ym xm r0 =
             state $ \(xState0,yState0) ->
                let (r1, xState1) = runState (xm r0) xState0
                    (r2, yState1) = runState (ym r1) yState0
                in  (r2, (xState1,yState1))
      in  Modulator
             (xInit,yInit)
             (compose yTime xTime)
             (compose yBody xBody)

instance Arrow Modulator where
   arr f = Modulator () return (return . f)
   first (Modulator xInit xTime xBody) =
      Modulator xInit xTime
         (\(a0,c) -> fmap (\a1 -> (a1,c)) $ xBody a0)


applyModulator ::
   Modulator a b ->
   EventList.T StrictTime [a] ->
   EventList.T StrictTime [b]
applyModulator
      (Modulator modulatorInit modulatorTime modulatorBody) =
   flip evalState modulatorInit .
   EventList.traverse modulatorTime (traverse modulatorBody)


{-# INLINE sequenceCore #-}
sequenceCore ::
   (Check.C event, Monoid signal) =>
   Channel ->
   Program ->
   Modulator Note signal ->
   FilterSequence event signal
sequenceCore chan initPgm md =
   fmap (EventList.mapTime fromIntegral .
         flatten .
         applyModulator md .
         matchNoteEvents .
         embedPrograms initPgm) $
   getNoteEvents chan


errorNoProgram :: Program
errorNoProgram =
   ChannelMsg.toProgram 0
{-
Since we compute the current program strictly in embedPrograms,
initializing with undefined does no longer work.
   error "MIDI program not initialized"
-}


{-# INLINE sequence #-}
sequence ::
   (Check.C event, Monoid signal, Trans.C y) =>
   Channel ->
   Instrument y signal ->
   FilterSequence event signal
sequence chan instr =
   sequenceCore chan errorNoProgram
      (Modulator () return
          (return . renderInstrumentIgnoreProgram instr))


{-# INLINE sequenceModulated #-}
sequenceModulated ::
   (Check.C event, CutG.Transform ctrl, CutG.NormalForm ctrl,
    Monoid signal, Trans.C y) =>
   ctrl ->
   Channel ->
   (ctrl -> Instrument y signal) ->
   FilterSequence event signal
sequenceModulated ctrl chan instr =
   sequenceCore chan errorNoProgram
      (Modulator ctrl advanceModulationChunk
          (\note -> gets $ \c -> renderInstrumentIgnoreProgram (instr c) note))


{-# INLINE sequenceMultiModulated #-}
sequenceMultiModulated ::
   (Check.C event, Monoid signal, Trans.C y) =>
   Channel ->
   instrument ->
   Modulator (instrument, Note) (Instrument y signal, Note) ->
   FilterSequence event signal
sequenceMultiModulated chan instr
      (Modulator modulatorInit modulatorTime modulatorBody) =
   sequenceCore chan errorNoProgram
      (Modulator modulatorInit modulatorTime
          (fmap (uncurry renderInstrumentIgnoreProgram) .
           modulatorBody .
           (,) instr))

{-# INLINE sequenceMultiProgram #-}
sequenceMultiProgram ::
   (Check.C event, Monoid signal, Trans.C y) =>
   Channel ->
   Program ->
   [Instrument y signal] ->
   FilterSequence event signal
sequenceMultiProgram chan initPgm instrs =
   let bank = makeInstrumentArray instrs
   in  sequenceCore chan initPgm
          (Modulator () return
              (return . renderInstrument
                 (getInstrumentFromArray bank initPgm)))

{-# INLINE sequenceModulatedMultiProgram #-}
sequenceModulatedMultiProgram ::
   (CutG.Transform ctrl, CutG.NormalForm ctrl,
    Check.C event, Monoid signal, Trans.C y) =>
   ctrl ->
   Channel ->
   Program ->
   [ctrl -> Instrument y signal] ->
   FilterSequence event signal
sequenceModulatedMultiProgram ctrl chan initPgm instrs =
   let bank = makeInstrumentArray instrs
   in  sequenceCore chan initPgm
          (Modulator
              ctrl advanceModulationChunk
              (\note -> gets $ \c -> renderInstrument
                 (\pgm -> getInstrumentFromArray bank initPgm pgm c) note))
