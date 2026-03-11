module Main where

import qualified Synthesizer.MIDI.PiecewiseConstant as PC

import qualified Synthesizer.MIDI.Generic as Gen
import Synthesizer.MIDI.Storable (
   Instrument, chunkSizesFromLazyTime, )

import qualified Synthesizer.Basic.Wave          as Wave
import qualified Synthesizer.Frame.Stereo        as Stereo

import Foreign.Storable (Storable, )

import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified Synthesizer.State.Signal      as SigS
import qualified Synthesizer.State.Oscillator  as OsciS
import qualified Synthesizer.State.Filter.NonRecursive as FiltNRS

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import Sound.MIDI.Message.Channel (Channel, )

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
-- import Data.EventList.Relative.MixedBody ((/.), (./), )

import Control.Monad.Trans.State (evalState, )
import Control.Monad (when, )

import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric (zero, (^?), )
import Prelude hiding (Real, break, )


import qualified System.IO as IO



channel :: Channel
channel = ChannelMsg.toChannel 0

sampleRate :: Num a => a
-- sampleRate = 24000
-- sampleRate = 48000
sampleRate = 44100

latency :: Int
latency = 0
-- latency = 256
-- latency = 1000

chunkSize :: SVL.ChunkSize
chunkSize = SVL.defaultChunkSize

consNone ::
   time ->
   EventList.T time [body] ->
   EventList.T time [body]
consNone t = EventList.cons t []

consSingle ::
   time -> body ->
   EventList.T time [body] ->
   EventList.T time [body]
consSingle t b = EventList.cons t [b]

emptys :: time -> EventList.T time [ChannelMsg.T]
emptys t =
   let evs = consNone t evs
   in  evs


type Real = Float


evaluatePrefix ::
   (Storable a, Additive.C a, Eq a) =>
   SVL.Vector a -> Bool
evaluatePrefix =
   (\x -> x==x) .
   SVL.foldl' (Additive.+) zero .
   SVL.take 100000


sequenceSpaceLeakEmpty :: Bool
sequenceSpaceLeakEmpty =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState (Gen.sequence channel
      (error "no sound" :: Instrument Real Real)) $
   emptys 10

sequenceSpaceLeakModulatedEmpty :: Bool
sequenceSpaceLeakModulatedEmpty =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1+) 0) channel
         (error "no sound" :: SigSt.T Real -> Instrument Real Real)) $
   emptys 10


sequenceSpaceLeakLazyDrop :: Bool
sequenceSpaceLeakLazyDrop =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (let fm y = (EventListBT.cons $! y) 10 (fm (2-y))
       in  Gen.sequenceModulated (fm 1) channel
              (error "no sound" ::
                  PC.T Real -> Instrument Real Real)) $
   emptys 10


stringStereoFM :: SigSt.T Real -> Instrument Real (Stereo.T Real)
stringStereoFM fmSt vel freq dur =
   let fm = SigS.fromStorableSignal fmSt
   in  SigS.toStorableSignalVary (chunkSizesFromLazyTime dur) $
       FiltNRS.amplifyVector (4^?vel) $
       SigS.zipWith Stereo.cons
          (OsciS.freqMod Wave.saw zero $
           FiltNRS.amplify (freq*0.999/sampleRate) fm)
          (OsciS.freqMod Wave.saw zero $
           FiltNRS.amplify (freq*1.001/sampleRate) fm)


makeNote ::
   (ChannelMsg.Pitch ->
    ChannelMsg.Velocity ->
    VoiceMsg.T) ->
   Int -> ChannelMsg.T
makeNote note pitch =
   ChannelMsg.Cons
      (ChannelMsg.toChannel 0)
      (ChannelMsg.Voice $
       note (VoiceMsg.toPitch pitch)
            VoiceMsg.normalVelocity)

makeNoteOn, makeNoteOff :: Int -> ChannelMsg.T
makeNoteOn  = makeNote VoiceMsg.NoteOn
makeNoteOff = makeNote VoiceMsg.NoteOff


sequenceSpaceLeakModulatedInfinite :: Bool
sequenceSpaceLeakModulatedInfinite =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1e-7 +) 1) channel
         stringStereoFM) $
   let evs t = consNone t (evs (20-t))
   in  consSingle 10 (makeNoteOn 60) $
       evs 10

sequenceSpaceLeakModulatedChordStep :: Bool
sequenceSpaceLeakModulatedChordStep =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1e-7 +) 1) channel
         stringStereoFM) $
   let evs t = consNone t (evs (20-t))
   in  consSingle 10 (makeNoteOn 60) $
       consSingle 10 (makeNoteOn 64) $
       consSingle 10 (makeNoteOn 67) $
       evs 10

sequenceSpaceLeakModulatedChordSimultaneous :: Bool
sequenceSpaceLeakModulatedChordSimultaneous =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1e-7 +) 1) channel
         stringStereoFM) $
   let evs t = EventList.cons t [] (evs (20-t))
   in  EventList.cons 10
          [makeNoteOn 60,
           makeNoteOn 64,
           makeNoteOn 67] $
       evs 10

sequenceSpaceLeakStaccato :: Bool
sequenceSpaceLeakStaccato =
   evaluatePrefix $
   CutSt.arrange chunkSize $
   evalState
      (Gen.sequenceModulated
         (SigSt.iterate chunkSize (1e-7 +) 1) channel
         stringStereoFM) $
   let evs t =
          consSingle t (makeNoteOn  60) $
          consSingle t (makeNoteOff 60) $
          evs (20-t)
   in  evs 10


test :: String -> Bool -> IO ()
test name result = do
   putStr name
   IO.hFlush IO.stdout
   when result (putStrLn " ok")

main :: IO ()
main = do
   test "sequenceSpaceLeakEmpty"                      sequenceSpaceLeakEmpty
   test "sequenceSpaceLeakModulatedEmpty"             sequenceSpaceLeakModulatedEmpty
   test "sequenceSpaceLeakLazyDrop"                   sequenceSpaceLeakLazyDrop
   test "sequenceSpaceLeakModulatedInfinite"          sequenceSpaceLeakModulatedInfinite
   test "sequenceSpaceLeakModulatedChordStep"         sequenceSpaceLeakModulatedChordStep
   test "sequenceSpaceLeakModulatedChordSimultaneous" sequenceSpaceLeakModulatedChordSimultaneous
   test "sequenceSpaceLeakStaccato"                   sequenceSpaceLeakStaccato
