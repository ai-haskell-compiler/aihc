{- |
Convert MIDI events of a MIDI controller to a control signal.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.Dimensional where

import qualified Sound.MIDI.Message.Class.Check as Check

import qualified Synthesizer.MIDI.PiecewiseConstant as MidiPC
import qualified Synthesizer.MIDI.EventList as MidiEL
import qualified Synthesizer.MIDI.Generic as MidiG
import Synthesizer.MIDI.EventList
          (Channel, Controller, Note(Note), Program, )
import Synthesizer.MIDI.Generic (errorNoProgram, )

import qualified Synthesizer.MIDI.Value as MV
import qualified Synthesizer.MIDI.Dimensional.Value as DMV

import qualified Synthesizer.Dimensional.Causal.Process    as Causal
import qualified Synthesizer.Dimensional.Causal.Filter     as Filt

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Rate.Oscillator as OsciR
import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Amplitude.Displacement as DispA
import qualified Synthesizer.Dimensional.Amplitude.Filter as FiltA
import qualified Synthesizer.Dimensional.Wave as WaveD

import qualified Synthesizer.Basic.Wave          as Wave

import Synthesizer.Dimensional.Causal.Process ((<<<), )
import Synthesizer.Dimensional.Process (($:), )

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.ChunkySize as ChunkySize

import qualified Synthesizer.Generic.Cut          as CutG
import qualified Synthesizer.Generic.Signal       as SigG
import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Data.EventList.Relative.TimeBody  as EventList

import Foreign.Storable (Storable, )

import qualified Number.NonNegative as NonNegW
import qualified Number.NonNegativeChunky as Chunky

import qualified Numeric.NonNegative.Chunky as Chunky98

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm as DN

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field

import Control.Category ((.), )
import Control.Applicative (Applicative, pure, (<*>), liftA2, )
import Control.Monad.Trans.State (State, evalState, state, gets, )
import Control.Monad (liftM, )

import NumericPrelude.Base hiding (id, (.), )
import NumericPrelude.Numeric
import Prelude (RealFrac, )


type Signal s v y signal =
   AmpSignal s (Amp.Dimensional v y) signal

type AmpSignal s amp signal =
   SigA.T (Rate.Phantom s) amp signal

{- |
This type ensures that all signals generated from the event list
share the same sample rate.
-}
newtype Filter event s u t a =
   Filter (MidiEL.Filter event (Proc.T s u t a))

{-# INLINE runFilter #-}
runFilter ::
   Check.C event =>
   EventList.T MidiEL.StrictTime [event] ->
   Filter event s u t a -> Proc.T s u t a
runFilter evs (Filter f) =
   evalState f evs

instance Functor (Filter event s u t) where
   fmap f (Filter flt) =
      Filter (fmap (fmap f) flt)

instance Applicative (Filter event s u t) where
   pure x = Filter (pure (pure x))
   Filter f <*> Filter x =
      Filter (liftA2 (<*>) f x)


{-# INLINE piecewiseConstant #-}
piecewiseConstant ::
   (SigG.Write sig y) =>
   SigA.T rate amp (MidiPC.T y) ->
   SigA.T rate amp (sig y)
piecewiseConstant =
   SigA.processBody MidiG.piecewiseConstant

{-# INLINE controllerLinear #-}
controllerLinear ::
   (Check.C event, Field.C y, Ord y, Dim.C u, Dim.C v) =>
   Channel -> Controller ->
   (DN.T v y, DN.T v y) -> DN.T v y ->
   Filter event s u t (Signal s v y (MidiPC.T y))
controllerLinear chan ctrl bnd initial =
   Filter $
   liftM
      (let amp = max initial (uncurry max bnd)
       in  return . SigA.fromBody amp .
           MidiPC.initWith
              (DMV.controllerLinear amp bnd) (DN.divToScalar initial amp)) $
   MidiEL.getControllerEvents chan ctrl


{-# INLINE controllerExponential #-}
controllerExponential ::
   (Check.C event, Trans.C y, Ord y, Dim.C u, Dim.C v) =>
   Channel -> Controller ->
   (DN.T v y, DN.T v y) -> DN.T v y ->
   Filter event s u t (Signal s v y (MidiPC.T y))
controllerExponential chan ctrl bnd initial =
   Filter $
   liftM
      (let amp = max initial (uncurry max bnd)
       in  return . SigA.fromBody amp .
           MidiPC.initWith
              (DMV.controllerExponential amp bnd) (DN.divToScalar initial amp)) $
   MidiEL.getControllerEvents chan ctrl


{- |
@pitchBend channel range center@:
emits frequencies on an exponential scale from
@center/range@ to @center*range@.
-}
{-# INLINE pitchBend #-}
pitchBend ::
   (Check.C event, Trans.C y, Ord y, Dim.C u, Dim.C v) =>
   Channel ->
   y -> DN.T v y ->
   Filter event s u t (Signal s v y (MidiPC.T y))
pitchBend chan range center =
   Filter $
   liftM
      (let amp = DN.scale (max range (recip range)) center
       in  return . SigA.fromBody amp .
           MidiPC.initWith
              (DMV.pitchBend amp range center) (DN.divToScalar center amp)) $
   MidiEL.getSlice (Check.pitchBend chan)
--   MidiEL.getPitchBendEvents chan


{-# INLINE channelPressure #-}
channelPressure ::
   (Check.C event, Trans.C y, Ord y, Dim.C u, Dim.C v) =>
   Channel ->
   DN.T v y -> DN.T v y ->
   Filter event s u t (Signal s v y (MidiPC.T y))
channelPressure chan maxVal initVal =
   Filter $
   liftM
      (return . SigA.fromBody maxVal .
       MidiPC.initWith
          (DMV.controllerLinear maxVal (zero,maxVal))
          (DN.divToScalar initVal maxVal)) $
   MidiEL.getSlice (Check.channelPressure chan)
--   MidiEL.getPitchBendEvents chan


{-# INLINE bendWheelPressure #-}
bendWheelPressure ::
   (Check.C event,
    SigG.Write sig q, SigG.Transform sig q,
    RealField.C q, Trans.C q, Module.C q q, Dim.C u) =>
   Channel ->
   Int -> DN.T (Dim.Recip u) q -> q -> q ->
   Filter event s u q (Signal s Dim.Scalar q (sig q))
bendWheelPressure chan
     pitchRange speed wheelDepth pressDepth =
   pure
      (\bend fm press  osci env ->
         let modu =
                DispA.raise 1 $
                FiltA.envelope
                   osci
                   (DispA.mix
                      (SigA.restore fm)
                      (SigA.restore press))

         in  Causal.apply
                (env <<< Causal.feedSnd modu <<< Causal.canonicalizeFlat)
                (piecewiseConstant bend))

    $: pitchBend chan (2^?(fromIntegral pitchRange/12)) (DN.scalar 1)
    $: controllerLinear chan VoiceMsg.modulation (zero, DN.scalar wheelDepth) zero
    $: channelPressure chan (DN.scalar pressDepth) 0

    $: Filter (return $ OsciR.static (WaveD.flat Wave.sine) zero speed)
    $: Filter (return $ Filt.envelope)


type LazyTime s = SigA.T (Rate.Phantom s) Amp.Abstract ChunkySize.T
-- type LazyTime s = SigA.T (Rate.Phantom s) Amp.Abstract SigStV.LazySize
-- type LazyTime s = SigA.T (Rate.Phantom s) Amp.Abstract MidiEL.LazyTime

type Instrument s u v q signal =
   ModulatedInstrument s u q (Signal s v q signal)

type ModulatedInstrument s u q signal =
   q -> DN.T (Dim.Recip u) q ->
   Proc.T s u q (LazyTime s -> signal)

type Bank s u q signal =
   Program -> ModulatedInstrument s u q signal


{-# INLINE chunkySizeFromLazyTime #-}
chunkySizeFromLazyTime :: MidiEL.LazyTime -> ChunkySize.T
chunkySizeFromLazyTime =
   Chunky.fromChunks .
   map (SigG.LazySize . NonNegW.toNumber) .
   concatMap PC.chopLongTime .
   Chunky98.toChunks .
   Chunky98.normalize


{-# INLINE renderInstrument #-}
renderInstrument ::
   (Trans.C q) =>
   Bank s Dim.Time q signal ->
   Note ->
   Proc.T s Dim.Time q signal
renderInstrument instrument (Note pgm pitch vel dur) =
   fmap ($ SigA.abstractFromBody $ chunkySizeFromLazyTime dur) $
   instrument pgm
      (MV.velocity vel)
      (DMV.frequencyFromPitch pitch)

{- |
Instrument parameters are:
velocity from -1 to 1
(0 is the normal pressure, no pressure aka NoteOff is not supported),
frequency is given in Hertz
-}
{-# INLINE makeInstrumentSounds #-}
makeInstrumentSounds ::
   (Trans.C q) =>
   Bank s Dim.Time q signal ->
   EventList.T time [Note] ->
   Proc.T s Dim.Time q (EventList.T time [signal])
makeInstrumentSounds bank =
   EventList.mapBodyM (mapM (renderInstrument bank))


{-# INLINE sequence #-}
sequence ::
   (Check.C event, RealFrac q, Storable y, Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   Instrument s Dim.Time v q (SigSt.T y) ->
   Filter event s Dim.Time q (Signal s v q (SigSt.T y))
sequence chunkSize amp chan instr =
   fmap (renderSequence chunkSize amp) $
   prepareTones chan errorNoProgram (const instr)

{-
{-# INLINE sequence #-}
sequence ::
   (Check.C event,
    RealFrac q, Storable y, Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   Instrument s Dim.Time v q y ->
   Filter event (Proc.T s Dim.Time q (Signal s v q (SigSt.T y)))
sequence chunkSize amp chan instr =
   fmap ((CutA.arrangeStorableVolume undefined  {- chunkSize -} amp undefined $:) .
         fmap
            (EventListTM.switchTimeR const .
             EventListTT.mapTime fromIntegral .
             insertBreaksGen (SigA.fromBody amp SigSt.empty)) .
         makeInstrumentSounds instr .
         MidiEL.matchNoteEvents) $
   MidiEL.getNoteEvents chan
-}


{-# INLINE sequenceModulated #-}
sequenceModulated ::
   (Check.C event,
    CutG.Transform ctrl,  CutG.NormalForm ctrl,
    RealFrac q, Storable y,
    Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   ModulatedInstrument s Dim.Time q
      (AmpSignal s amp ctrl -> Signal s v q (SigSt.T y)) ->
   Filter event s Dim.Time q
      (AmpSignal s amp ctrl -> Signal s v q (SigSt.T y))
sequenceModulated chunkSize amp chan instr =
   fmap (flip $ \ctrl ->
      renderSequence chunkSize amp .
      applyModulator (applyModulation ctrl)) $
   prepareTones chan errorNoProgram (const instr)

{-# INLINE sequenceModulated2 #-}
sequenceModulated2 ::
   (Check.C event,
    CutG.Transform ctrl0, CutG.NormalForm ctrl0,
    CutG.Transform ctrl1, CutG.NormalForm ctrl1,
    RealFrac q, Storable y,
    Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   ModulatedInstrument s Dim.Time q
      (AmpSignal s amp0 ctrl0 -> AmpSignal s amp1 ctrl1 -> Signal s v q (SigSt.T y)) ->
   Filter event s Dim.Time q
      (AmpSignal s amp0 ctrl0 -> AmpSignal s amp1 ctrl1 -> Signal s v q (SigSt.T y))
sequenceModulated2 chunkSize amp chan instr =
   fmap (\evs ctrl0 ctrl1 ->
      renderSequence chunkSize amp .
      applyModulator
         (applyModulation ctrl1 .
          applyModulation ctrl0)
      $ evs) $
   prepareTones chan errorNoProgram (const instr)


{-# INLINE sequenceMultiModulated #-}
sequenceMultiModulated ::
   (Check.C event, RealFrac q, Storable y,
    Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   ModulatedInstrument s Dim.Time q instrument ->
   Filter event s Dim.Time q
      (MidiG.Modulator instrument (Signal s v q (SigSt.T y))) ->
   Filter event s Dim.Time q (Signal s v q (SigSt.T y))
sequenceMultiModulated chunkSize amp chan instr modu =
   fmap (renderSequence chunkSize amp) $
   (fmap applyModulator modu $:
    prepareTones chan errorNoProgram (const instr))

{-# INLINE prepareTones #-}
prepareTones ::
   (Check.C event, RealFrac q, Trans.C q) =>
   -- ToDo: use time value
   Channel ->
   Program ->
   Bank s Dim.Time q signal ->
   Filter event s Dim.Time q (EventList.T MidiEL.StrictTime [signal])
prepareTones chan initPgm instr =
   Filter $
   fmap (makeInstrumentSounds instr .
         MidiEL.matchNoteEvents .
         MidiEL.embedPrograms initPgm) $
   MidiEL.getNoteEvents chan

{-# INLINE applyModulation #-}
applyModulation ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   AmpSignal s amp signal ->
   MidiG.Modulator (AmpSignal s amp signal -> body) body
applyModulation ctrl =
   MidiG.Modulator ctrl advanceModulationChunk gets

{-# INLINE applyModulator #-}
applyModulator ::
   MidiG.Modulator a b ->
   EventList.T MidiEL.StrictTime [a] ->
   EventList.T MidiEL.StrictTime [b]
applyModulator =
   MidiG.applyModulator

{-# INLINE renderSequence #-}
renderSequence ::
   (Storable y, Module.C q y, Dim.C u, Field.C q) =>
   SVL.ChunkSize ->
   DN.T u q ->
   EventList.T MidiEL.StrictTime [Signal s u q (SigSt.T y)] ->
   Signal s u q (SigSt.T y)
renderSequence chunkSize amp =
   SigA.fromBody amp .
   CutSt.arrangeEquidist chunkSize .
   {- This concatenates times across empty events,
      and thus is too strict.
   EventList.flatten .
   -}
   MidiG.flatten .
   EventList.mapTime fromIntegral .
   EventList.mapBody (map (SigA.vectorSamples (flip DN.divToScalar amp)))


{-# INLINE advanceModulationChunky #-}
advanceModulationChunky ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   MidiEL.LazyTime -> State (AmpSignal s amp signal) MidiEL.LazyTime
advanceModulationChunky =
   liftM Chunky98.fromChunks .
   mapM advanceModulationChunk .
   Chunky98.toChunks

{-# INLINE advanceModulationChunk #-}
advanceModulationChunk ::
   (CutG.Transform signal, CutG.NormalForm signal) =>
   MidiEL.StrictTime -> State (AmpSignal s amp signal) MidiEL.StrictTime
advanceModulationChunk t = state $ \xs ->
   let ys = SigA.processBody (CutG.drop (fromIntegral t)) xs
   in  (MidiG.evaluateVectorHead (SigA.body ys) t, ys)


{-# INLINE sequenceMultiProgram #-}
sequenceMultiProgram ::
   (Check.C event, RealFrac q, Storable y, Module.C q y, Trans.C q, Dim.C v) =>
   SVL.ChunkSize ->
   DN.T v q ->
   Channel ->
   Program ->
--   Bank s Dim.Time q (Signal s v q (SigSt.T y)) ->
   [Instrument s Dim.Time v q (SigSt.T y)] ->
   Filter event s Dim.Time q (Signal s v q (SigSt.T y))
sequenceMultiProgram chunkSize amp chan initPgm instrs =
   let bank = MidiEL.makeInstrumentArray instrs
   in  fmap (renderSequence chunkSize amp) $
       prepareTones chan initPgm $
       MidiEL.getInstrumentFromArray bank initPgm
