module Synthesizer.ALSA.CausalIO.Process (
   Events,
   playFromEvents,
   Output,
   playFromEventsWithParams,
   ) where

import qualified Synthesizer.ALSA.EventList as MIDIEv

import qualified Synthesizer.ALSA.Storable.Play as Play
import Synthesizer.MIDI.EventList (StrictTime, )

import qualified Synthesizer.CausalIO.Process as PIO

import qualified Sound.ALSA.PCM as PCM
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Data.EventList.Relative.TimeTime  as EventListTT

import qualified Algebra.RealField      as RealField
import qualified Algebra.Additive       as Additive

import qualified Data.StorableVector as SV

import Control.Exception (bracket, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


type Events = EventListTT.T StrictTime [Event.T]

playFromEvents ::
   (RealField.C time, PCM.SampleFmt a, Additive.C a) =>
   Play.Device -> MIDIEv.ClientName -> time -> time -> PCM.SampleFreq ->
   PIO.T Events (SV.Vector a) ->
   IO ()
playFromEvents device name latency beat rate
      (PIO.Cons next create delete) =
   let sink = Play.makeSink device beat rate
       rateFloat = fromIntegral rate
   in  MIDIEv.withMIDIEventsChunked name beat rateFloat $ \getEventsList ->
       PCM.withSoundSink sink $ \to ->
{-
       Play.writeLazy sink to
          (SVL.replicate
              (SVL.chunkSize $ round (beat * rateFloat))
              (round (latency * rateFloat))
              (zero::Float))
-}
       Play.write sink to
          (SV.replicate (round (latency * rateFloat)) zero) >>
       (bracket create delete $ \state ->
        let loop getEvs0 s0 =
               case getEvs0 of
                  [] -> return ()
                  getEvents : getEvs1 -> do
                     evs <- getEvents
                     (pcm, s1) <- next evs s0
                     Play.write sink to pcm
                     loop getEvs1 s1
        in  loop getEventsList state)


type Output handle signal a =
   (IO ((PCM.Size, PCM.SampleFreq), handle),
    handle -> IO (),
    handle -> signal -> IO a)

playFromEventsWithParams ::
   Output handle signal () ->
   MIDIEv.ClientName ->
   ((PCM.Size, PCM.SampleFreq) -> PIO.T Events signal) ->
   IO ()
playFromEventsWithParams (open, close, write) name process =
   bracket open (close . snd) $ \(p@(period,rate),h) ->
      let rateFloat = fromIntegral rate :: Double
          beat = fromIntegral period / rateFloat
      in  MIDIEv.withMIDIEventsChunked name beat rateFloat $ \getEventsList ->
             case process p of
                PIO.Cons next create delete -> do
{-
                   write
                      (SV.replicate (round (latency * rateFloat)) zero)
-}
                   bracket create delete $ \state ->
                      let loop getEvs0 s0 =
                             case getEvs0 of
                                [] -> return ()
                                getEvents : getEvs1 -> do
                                   evs <- getEvents
                                   (chunk, s1) <- next evs s0
                                   write h chunk
                                   loop getEvs1 s1
                      in  loop getEventsList state
