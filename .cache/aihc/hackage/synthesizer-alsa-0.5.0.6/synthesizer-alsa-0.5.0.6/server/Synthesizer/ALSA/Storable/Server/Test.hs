module Synthesizer.ALSA.Storable.Server.Test where

import qualified Synthesizer.MIDI.Example.Instrument as Instr
import Synthesizer.ALSA.Storable.Server.Common
          (Real, withMIDIEvents, play,
           sampleRate, chunkSize, channel, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Synthesizer.MIDI.PiecewiseConstant as PC
import qualified Synthesizer.MIDI.Generic as Gen
import qualified Synthesizer.MIDI.Storable as MidiSt
import Synthesizer.MIDI.Storable (
   Instrument, chunkSizesFromLazyTime, )

import qualified Synthesizer.MIDI.EventList as MIDIEv
import Synthesizer.MIDI.EventList (
   LazyTime, StrictTime, Note(..), NoteBoundary(..),
   matchNoteEvents, getSlice, getControllerEvents, )

import qualified Synthesizer.Basic.Wave          as Wave

import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((<<<), )

import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Synthesizer.Storable.Oscillator  as OsciSt
import qualified Synthesizer.Storable.Signal      as SigSt
-- import qualified Data.StorableVector.Lazy.Builder as Bld
import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy         as SVL
import qualified Data.StorableVector              as SV

import qualified Synthesizer.State.Signal      as SigS

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import Sound.MIDI.Message.Channel.Voice (normalVelocity, )

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import Data.EventList.Relative.MixedBody ((/.), (./), )

import qualified Control.Monad.Trans.State.Strict as MS
import Control.Monad.Trans.State (evalState, gets, )
import Control.Category ((.), )

import Data.Traversable (traverse, )

-- import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import Data.Maybe.HT (toMaybe, )

import NumericPrelude.Numeric (zero, round, (^?), )
import Prelude hiding (Real, round, break, id, (.), )



frequency1 :: IO ()
frequency1 =
   withMIDIEvents play $
      const
        (OsciSt.static chunkSize Wave.sine zero (800/sampleRate::Real))

frequency2 :: IO ()
frequency2 =
   withMIDIEvents (const $ const print) $
      evalState (getControllerEvents channel VoiceMsg.mainVolume)

frequency3 :: IO ()
frequency3 =
   withMIDIEvents (const $ const print) $
      evalState (getSlice Just)


keyboard1 :: IO ()
keyboard1 =
   withMIDIEvents play $
      const (Instr.ping 0 440)

keyboard2 :: SigSt.T Real
keyboard2 =
   let music :: Real -> EventList.T StrictTime (SigSt.T Real)
       music x = 5 /. SigSt.replicate chunkSize 6 x ./ music (x+1)
   in  CutSt.arrange chunkSize $
       EventList.mapTime fromIntegral $ music 42

keyboard3 :: SigSt.T Real
keyboard3 =
   let time :: Real -> Int
       time t = round (t * sampleRate)
       music :: Real -> EventList.T StrictTime (SigSt.T Real)
       music x =
          fromIntegral (time 0.2) /.
          SigSt.take (time 0.4) (Instr.ping 0 x) ./
          music (x*1.01)
   in  CutSt.arrange chunkSize $
       EventList.mapTime fromIntegral $ music 110

makeLazyTime :: Real -> LazyTime
makeLazyTime t =
   NonNegChunky.fromNumber $
   NonNegW.fromNumberMsg "keyboard time" $
   round (t * sampleRate)

makeStrictTime :: Real -> StrictTime
makeStrictTime t =
   NonNegW.fromNumberMsg "keyboard time" $
   round (t * sampleRate)

pitch :: Int -> VoiceMsg.Pitch
pitch = VoiceMsg.toPitch

defaultProgram :: VoiceMsg.Program
defaultProgram = VoiceMsg.toProgram 0

embedDefaultProgram ::
   EventList.T StrictTime [NoteBoundary Bool] ->
   EventList.T StrictTime [NoteBoundary (Maybe VoiceMsg.Program)]
embedDefaultProgram =
   fmap (fmap (\(NoteBoundary p v b) ->
      NoteBoundary p v (toMaybe b defaultProgram)))

keyboard4 :: SigSt.T Real
keyboard4 =
   let {-
       idInstr :: Real -> Real -> SigSt.T Real
       idInstr _vel freq = SigSt.repeat chunkSize freq
       -}
--       inf = time 0.4 + inf
       music :: Int -> EventList.T StrictTime Note
       music p =
          makeStrictTime 0.2 /.
--          (pitch p, normalVelocity, inf) ./
          Note defaultProgram (pitch p) normalVelocity (makeLazyTime 0.4) ./
          music (p+1)
   in  CutSt.arrange chunkSize $
       EventList.mapTime fromIntegral $
       fmap (Gen.renderInstrumentIgnoreProgram Instr.pingDur) $
       music 0


notes0 :: Int -> EventList.T StrictTime (NoteBoundary Bool)
notes0 p =
   makeStrictTime 0.2 /.
   (let (oct,pc) = divMod p 12
    in  (NoteBoundary (pitch (50 + pc)) normalVelocity (even oct)))
      ./
   notes0 (p+1)

notes1 :: EventList.T StrictTime (NoteBoundary Bool)
notes1 =
   makeStrictTime 0.2 /.
   (NoteBoundary (pitch 50) normalVelocity True) ./
   makeStrictTime 0.2 /.
   (NoteBoundary (pitch 52) normalVelocity True) ./
   makeStrictTime 0.2 /.
   (NoteBoundary (pitch 54) normalVelocity True) ./
   makeStrictTime 0.2 /.
--   (NoteBoundary (pitch 50) normalVelocity False) ./
   undefined

notes2 :: EventList.T StrictTime [NoteBoundary Bool]
notes2 =
   makeStrictTime 0.2 /.
   [] ./
   makeStrictTime 0.2 /.
   [] ./
   makeStrictTime 0.2 /.
   [NoteBoundary (pitch 50) normalVelocity True] ./
   makeStrictTime 0.2 /.
   [NoteBoundary (pitch 52) normalVelocity True] ./
   makeStrictTime 0.2 /.
   [NoteBoundary (pitch 54) normalVelocity True] ./
   makeStrictTime 0.2 /.
   [NoteBoundary (pitch 50) normalVelocity False] ./
   undefined

notes3 :: EventList.T StrictTime [NoteBoundary (Maybe VoiceMsg.Program)]
notes3 =
   embedDefaultProgram $
   notes2

keyboard5 :: SigSt.T Real
keyboard5 =
   CutSt.arrange chunkSize $
   EventList.mapTime fromIntegral $
   Gen.flatten $
   fmap (map (Gen.renderInstrumentIgnoreProgram Instr.pingDur)) $
   matchNoteEvents $
   notes3

keyboard6 :: EventList.T StrictTime [Note]
keyboard6 =
   matchNoteEvents $
   embedDefaultProgram $
   fmap (:[]) $
   notes1

keyboard7 :: EventList.T StrictTime [(VoiceMsg.Pitch, VoiceMsg.Velocity)]
keyboard7 =
   fmap (map (\ ~(Note _ p v _d) -> (p,v))) $
   keyboard6


emptyEvents :: StrictTime -> EventList.T StrictTime [Event.T]
emptyEvents time =
   let evs = EventList.cons time [] evs
   in  evs


arrangeSpaceLeak0 :: IO ()
arrangeSpaceLeak0 =
   SVL.writeFile "test.f32" $
   CutSt.arrange chunkSize $
   evalState (Gen.sequence channel
      (error "no sound" :: Instrument Real Real)) $
   emptyEvents 10

arrangeSpaceLeak1 :: IO ()
arrangeSpaceLeak1 =
   SVL.writeFile "test.f32" $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1+) 0) channel
         (error "no sound" :: SigSt.T Real -> Instrument Real Real)) $
   emptyEvents 10

makeNote :: Event.NoteEv -> Event.Pitch -> Event.T
makeNote typ pit =
   Event.simple Addr.subscribers $ Event.NoteEv typ $
   Event.simpleNote (Event.Channel 0) pit Event.normalVelocity

{-
a space leak can only be observed for more than one note,
maybe our 'break' improvement fixed the case for one played note
-}
arrangeSpaceLeak3 :: IO ()
arrangeSpaceLeak3 =
   SVL.writeFile "test.f32" $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1e-7 +) 1) channel
         Instr.stringStereoFM) $
--         (const Instr.pingDur :: SigSt.T Real -> Instrument Real Real)) $
   let evs t = EventList.cons t ([]::[Event.T]) (evs (20-t))
   in  -- EventList.cons 10 [makeNote MIDI.NoteOn 60] $
       -- EventList.cons 10 [makeNote MIDI.NoteOn 64] $
       evs 10

arrangeSpaceLeak4 :: IO ()
arrangeSpaceLeak4 =
   SVL.writeFile "test.f32" $
   evalState
      (do bend <- MidiSt.pitchBend channel (2^?(2/12)) 1
          MidiSt.sequenceModulated chunkSize bend channel Instr.stringStereoFM) $
   let evs t = EventList.cons t ([]::[Event.T]) (evs (20-t))
   in  evs 10

chordSpaceLeak1 :: IO ()
chordSpaceLeak1 =
   SVL.writeFile "test.f32" $
   CutSt.arrange chunkSize $
   evalState (Gen.sequence channel Instr.pingDur) $
   let evs t = EventList.cons t [] (evs (20-t))
   in  EventList.cons 10 [makeNote Event.NoteOn $ Event.Pitch 60] $
       EventList.cons 10 [makeNote Event.NoteOn $ Event.Pitch 64] $
       evs 10


sequencePitchBend :: IO ()
sequencePitchBend =
   SVL.writeFile "test.f32" $
      CutSt.arrange chunkSize $
      evalState
         (let fm y = (EventListBT.cons $! y) 10 (fm (2-y))
          in  Gen.sequenceModulated (fm 1) channel
                 (error "no sound" ::
                     PC.T Real -> Instrument Real Real)) $
      emptyEvents 10

sequencePitchBend1 :: IO ()
sequencePitchBend1 =
   SVL.writeFile "test.f32" $
      CutSt.arrange chunkSize $
      evalState
         (let fm y = EventListBT.cons y 10 (fm (2-y))
              instr :: PC.T Real -> Instrument Real Real
              instr = error "no sound"
          in  Gen.sequenceCore
                 channel Gen.errorNoProgram
                 (Gen.Modulator (fm 1) Gen.advanceModulationChunk
                     (\note -> gets $ \c ->
                         Gen.renderInstrumentIgnoreProgram (instr c) note))) $
      emptyEvents 10

sequencePitchBend2 :: IO ()
sequencePitchBend2 =
   SVL.writeFile "test.f32" $
      let fm y = EventListBT.cons y 10 (fm (2-y))
          -- fm = EventListBT.cons 1 10 fm
          instr :: PC.T Real -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
          md =
             Gen.Modulator
                (fm 1)
                Gen.advanceModulationChunkPC
                -- Gen.advanceModulationChunk
                (\note -> gets $ \c ->
                    Gen.renderInstrumentIgnoreProgram (instr c) note)
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          Gen.applyModulator md $
          evs

sequencePitchBend3 :: IO ()
sequencePitchBend3 =
   SVL.writeFile "test.f32" $
      let fm y = EventListBT.cons y 10 (fm (2-y))
          -- fm = EventListBT.cons 1 10 fm
          instr :: PC.T Real -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
          modEvent note =
             gets $ \c ->
                Gen.renderInstrumentIgnoreProgram (instr c) note
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          flip evalState (fm 1) .
          EventList.traverse
             Gen.advanceModulationChunk
             (traverse modEvent) $
          evs

sequencePitchBend4 :: IO ()
sequencePitchBend4 =
   SVL.writeFile "test.f32" $
      let fm y = y : fm (2-y)
          -- fm = repeat 1
          instr :: [Real] -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
          modEvent note =
             gets $ \c ->
                Gen.renderInstrumentIgnoreProgram (instr c) note
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          flip evalState (fm 1) .
          EventList.traverse
             Gen.advanceModulationChunk
             (traverse modEvent) $
          evs

sequencePitchBend4a :: IO ()
sequencePitchBend4a =
   SVL.writeFile "test.f32" $
      let fm y = y : fm (2-y)
          -- fm = repeat 1
          instr :: [Real] -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
          modEvent note =
             MS.gets $ \c ->
                Gen.renderInstrumentIgnoreProgram (instr c) note
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          flip MS.evalState (fm 1) .
          EventList.traverse
             Gen.advanceModulationChunkStrict
             (traverse modEvent) $
          evs

sequencePitchBend4b :: IO ()
sequencePitchBend4b =
   SVL.writeFile "test.f32" $
      let fm y = y : fm (2-y)
          -- fm = repeat 1
          instr :: [Real] -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
      in  CutSt.arrange chunkSize .
          Gen.flatten $
          EventList.foldrPair
             (\t bs0 go s0 ->
                let s1 = tail s0
                    bs1 =
                       map (Gen.renderInstrumentIgnoreProgram (instr s1)) bs0
                in  EventList.cons
                       (if null s1 then t else t) bs1 $
                    go s1)
             (const EventList.empty) evs (fm 1)

sequencePitchBend4c :: IO ()
sequencePitchBend4c =
   SVL.writeFile "test.f32" $
      let fm y = y : fm (2-y)
          -- fm = repeat 1
          instr :: [Real] -> Instrument Real Real
          instr = error "no sound"
      in  CutSt.arrange chunkSize .
          Gen.flatten .
          EventList.fromPairList $
          foldr
             (\(t,bs0) go s0 ->
                let s1 = tail s0
                    bs1 =
                       map (Gen.renderInstrumentIgnoreProgram (instr s1)) bs0
                in  (if null s1 then t else t, bs1) :
                    go s1)
             (const [])
             (repeat (10,[]))
             (fm 1)

sequencePitchBend4d :: IO ()
sequencePitchBend4d =
   SVL.writeFile "test.f32" $
      let fm y = y : fm (2-y)
          -- fm = repeat 1
      in  CutSt.arrange chunkSize .
          EventList.fromPairList $
          foldr
             (\(t,b) go s0 ->
                let s1 = tail s0
                in  (if null s1 then t else t,
                     if null s1 then b else b) :
                    go s1)
             (const [])
             (repeat (10, SigSt.empty :: SigSt.T Real))
             (fm 1 :: [Real])

sequencePitchBend4e :: IO ()
sequencePitchBend4e =
   writeFile "test.txt" $
   foldr
      (\c go s0 ->
         let s1 = tail s0
         in  (if null s1 then c else c) :
             go s1)
      (const [])
      (repeat 'a')
      (iterate not False)
      -- (repeat True)

sequencePitchBend5 :: IO ()
sequencePitchBend5 =
   SVL.writeFile "test.f32" $
      let fm y = SigSt.iterate (SVL.ChunkSize 1) (y+) 0
          instr :: SigSt.T Real -> Instrument Real Real
          instr = error "no sound"
          evs = EventList.cons 10 [] evs
          modEvent note =
             gets $ \c ->
                Gen.renderInstrumentIgnoreProgram (instr c) note
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          flip evalState (fm 1e-6) .
          EventList.traverse
             Gen.advanceModulationChunk
             (traverse modEvent) $
          evs

dummySound :: Instrument Real Real
dummySound =
   \vel freq dur ->
      SigStV.take (chunkSizesFromLazyTime dur) $
      SigSt.repeat chunkSize (vel + 1e-3*freq)

sequenceStaccato :: IO ()
sequenceStaccato =
   SVL.writeFile "test.f32" $
      let evs t =
             EventList.cons t [Right $ NoteBoundary (pitch 60) normalVelocity True] $
             EventList.cons t [Right $ NoteBoundary (pitch 60) normalVelocity False] $
             evs (20-t)
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          EventList.mapBody
             (map (Gen.renderInstrumentIgnoreProgram dummySound)) .
          MIDIEv.matchNoteEvents .
          MIDIEv.embedPrograms defaultProgram $
          evs 10

sequenceStaccato3 :: IO ()
sequenceStaccato3 =
   SVL.writeFile "test.f32" $
      let evs t =
             EventList.cons t [NoteBoundary (pitch 60) normalVelocity (Just defaultProgram)] $
             EventList.cons t [NoteBoundary (pitch 60) normalVelocity Nothing] $
             evs (20-t)
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          EventList.mapBody
             (map (Gen.renderInstrumentIgnoreProgram dummySound)) .
          MIDIEv.matchNoteEvents $
          evs 10

sequenceStaccato2 :: IO ()
sequenceStaccato2 =
   SVL.writeFile "test.f32" $
      let p = Event.Pitch 60
          evs t =
             EventList.cons t [makeNote Event.NoteOn  p] $
             EventList.cons t [makeNote Event.NoteOff p] $
             evs (20-t)
      in  CutSt.arrange chunkSize .
          EventList.mapTime fromIntegral .
          Gen.flatten .
          EventList.mapBody
             (map (Gen.renderInstrumentIgnoreProgram dummySound)) .
          MIDIEv.matchNoteEvents .
          MIDIEv.embedPrograms defaultProgram .
          evalState (MIDIEv.getNoteEvents channel) $
          evs 10

sequenceStaccato1 :: IO ()
sequenceStaccato1 =
   SVL.writeFile "test.f32" $
      CutSt.arrange chunkSize $
      evalState (Gen.sequence channel dummySound) $
      let p = Event.Pitch 60
          evs t =
             EventList.cons t [makeNote Event.NoteOn  p] $
             EventList.cons t [makeNote Event.NoteOff p] $
             evs (20-t)
      in  evs 10


speed :: IO ()
speed =
   let _sig =
          Causal.apply
             (Instr.softStringCausalProcess 440 <<<
              Instr.softStringReleaseEnvelopeCausalProcess 0)
             (SigS.repeat True)
       sig =
          Causal.apply
             (Instr.softStringCausalProcess 440)
             (SigS.repeat 1)
   in  SV.writeFile "speed.f32" $
       SigS.runViewL sig
       (\next s -> fst $ SV.unfoldrN 1000000 next s)

speedChunky :: IO ()
speedChunky =
   let sig =
          Causal.apply
             (Instr.softStringCausalProcess 440 <<<
              Instr.softStringReleaseEnvelopeCausalProcess 0)
             (SigS.repeat True)
   in  SVL.writeFile "speed.f32" $
       SigSt.take 1000000 $
       SigS.toStorableSignal (SVL.chunkSize 100) sig
{-
       SigS.runViewL sig
       (\next s -> SVL.take 1000000 (SVL.unfoldr (SVL.chunkSize 100) next s))
-}

speedArrange :: IO ()
speedArrange =
   let sig =
          Causal.apply
             (Instr.softStringCausalProcess 440 <<<
              Instr.softStringReleaseEnvelopeCausalProcess 0)
             (SigS.repeat True)
       sigSt =
          SigS.toStorableSignal (SVL.chunkSize 100) sig
   in  SVL.writeFile "speed.f32" $
       SigSt.take 1000000 $
       CutSt.arrangeEquidist (SVL.chunkSize 100) $
       EventList.fromPairList [(10000,sigSt)]
