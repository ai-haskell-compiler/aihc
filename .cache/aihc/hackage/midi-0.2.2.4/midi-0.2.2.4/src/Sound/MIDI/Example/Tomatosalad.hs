{-
The famous song, that is named "Tomatensalat" in German.
-}
module Main where

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Save as Save

import Sound.MIDI.File (ElapsedTime, )

import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event      as Event

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Data.ByteString.Lazy as B
import Data.Tuple.HT (mapFst, )

import qualified Control.Monad.Trans.State as State
import Control.Monad (liftM3, )


g0,a0,b0,c1,cs1,d1,e1,f1,g1,a1,as1 :: VoiceMsg.Pitch
[g0,a0,b0,c1,cs1,d1,e1,f1,g1,a1,as1] =
   map VoiceMsg.toPitch [55,57,59,60,61,62,64,65,67,69,70]

melody :: [(VoiceMsg.Pitch, ElapsedTime)]
melody =
   let n p t = (VoiceMsg.increasePitch (-12) p, t)
   in  n g0 3 :
       n c1 1 :
       n c1 1 :
       n c1 1 :
       n e1 1 :
       n d1 1 :
       n c1 1 :
       n d1 1 :
       n g0 1 :
       n g0 1 :
       n g0 3 :
       n c1 1 :
       n c1 1 :
       n c1 1 :
       n e1 1 :
       n d1 1 :
       n c1 1 :
       n g1 3 :
       n e1 3 :
       n f1 1 :
       n f1 1 :
       n f1 1 :
       n a1 1 :
       n g1 1 :
       n f1 1 :
       n e1 1 :
       n e1 1 :
       n e1 1 :
       n g1 1 :
       n f1 1 :
       n e1 1 :
       n d1 1 :
       n d1 1 :
       n d1 1 :
       n f1 1 :
       n e1 1 :
       n d1 1 :
       n c1 3 :
       []


melodyEvents ::
   [(VoiceMsg.Pitch, ElapsedTime)] -> Int ->
   EventListBT.T ElapsedTime Event.T
melodyEvents mel pn =
   let chan = ChannelMsg.toChannel 0
       vel = VoiceMsg.toVelocity (VoiceMsg.normalVelocity+25)
       event =
          Event.MIDIEvent .
          ChannelMsg.Cons chan .
          ChannelMsg.Voice
   in  EventListBT.fromPairList $
       concatMap (\(pgm, (n,t)) ->
          [(event $ VoiceMsg.ProgramChange pgm, 0),
           (event $ VoiceMsg.NoteOn  n vel, t),
           (event $ VoiceMsg.NoteOff n vel, 0)]) $
       zip (cycle $ map VoiceMsg.toProgram [pn..(pn+4)]) $
       concat $ replicate 5 $ mel

solo :: Int -> MidiFile.T
solo pn =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 4)
      [EventList.cons 0
          (Event.MetaEvent $ MetaEvent.SetTempo 1500000) $
       EventListTM.switchTimeR const $
       EventListMT.consTime 0 $
       melodyEvents melody pn]


melody2 :: [(VoiceMsg.Pitch, ElapsedTime)]
melody2 =
   let n p t = (p, t)
   in  n g0 3 :
       n c1 1 :
       n c1 1 :
       n c1 1 :
       n e1 1 :
       n d1 1 :
       n c1 1 :
       n d1 1 :
       n g0 1 :
       n g0 1 :
       n g0 3 :
       n d1 1 :
       n d1 1 :
       n d1 1 :
       n f1 1 :
       n e1 1 :
       n d1 1 :
       n g1 3 :
       n e1 3 :
       n f1 1 :
       n f1 1 :
       n f1 1 :
       n a1 1 :
       n g1 1 :
       n f1 1 :
       n e1 1 :
       n e1 1 :
       n e1 1 :
       n g1 1 :
       n f1 1 :
       n e1 1 :
       n d1 1 :
       n d1 1 :
       n d1 1 :
       n f1 1 :
       n e1 1 :
       n d1 1 :
       n c1 3 :
       []

nextProgram :: State.State [VoiceMsg.Program] VoiceMsg.Program
nextProgram = State.state $ \(pgm:pgms) -> (pgm,pgms)

accompEvents :: Int -> EventListBT.T ElapsedTime Event.T
accompEvents pn =
   let chan = ChannelMsg.toChannel 0
       vel = VoiceMsg.toVelocity (VoiceMsg.normalVelocity-25)
       event =
          Event.MIDIEvent .
          ChannelMsg.Cons chan .
          ChannelMsg.Voice
       chord ::
          VoiceMsg.Pitch ->
          (VoiceMsg.Pitch, VoiceMsg.Pitch) ->
          State.State [VoiceMsg.Program] [(Event.T, ElapsedTime)]
       chord a_ (b_,c_) =
          liftM3
             (\ pgm0 pgm1 pgm2 ->
                let a = VoiceMsg.increasePitch (-12) a_
                    b = VoiceMsg.increasePitch (-12) b_
                    c = VoiceMsg.increasePitch (-12) c_
                in  map (mapFst event) $
                    (VoiceMsg.ProgramChange pgm0, 0) :
                    (VoiceMsg.NoteOn  a vel, 1) :
                    (VoiceMsg.NoteOff a vel, 0) :
                    (VoiceMsg.ProgramChange pgm1, 0) :
                    (VoiceMsg.NoteOn  b vel, 0) :
                    (VoiceMsg.NoteOn  c vel, 1) :
                    (VoiceMsg.NoteOff b vel, 0) :
                    (VoiceMsg.NoteOff c vel, 0) :
                    (VoiceMsg.ProgramChange pgm2, 0) :
                    (VoiceMsg.NoteOn  b vel, 0) :
                    (VoiceMsg.NoteOn  c vel, 1) :
                    (VoiceMsg.NoteOff b vel, 0) :
                    (VoiceMsg.NoteOff c vel, 0) :
                    [])
             nextProgram nextProgram nextProgram
       introChords =
          chord c1  (e1, g1) :
          chord g0  (e1, g1) :
          chord c1  (e1, g1) :
          chord g0  (e1, g1) :
          []
       chords =
          chord c1  (e1, g1) :
          chord g0  (e1, g1) :
          chord b0  (d1, g1) :
          chord g0  (d1, g1) :
          chord b0  (f1, g1) :
          chord g0  (f1, g1) :
          chord c1  (e1, g1) :
          chord g0  (g1, as1) :
          chord c1  (f1, a1) :
          chord a0  (f1, a1) :
          chord c1  (e1, g1) :
          chord cs1 (e1, a1) :
          chord b0  (d1, g1) :
          chord g0  (d1, g1) :
          chord c1  (e1, g1) :
          chord g0  (e1, g1) :
          []
   in  EventListBT.fromPairList $
       concat $ State.evalState
          (sequence $ concat $ introChords : replicate 5 chords) $
       cycle $ map VoiceMsg.toProgram [pn..(pn+4)]

song :: Int -> Int -> MidiFile.T
song pna pnm =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 4)
      [EventList.cons 0
          (Event.MetaEvent $ MetaEvent.SetTempo 1500000) $
       let tb t =
              EventListTM.switchTimeR const .
              EventListMT.consTime t
       in  EventList.mergeBy (\ _ _ -> True)
              (tb 0 $ accompEvents pna) (tb 9 $ melodyEvents melody2 pnm)]

main :: IO ()
main =
   B.writeFile "tomatosalad.mid" (Save.toByteString (solo 16)) >>
   B.writeFile "hal.mid"         (Save.toByteString (solo 21)) >>
   B.writeFile "graphtheory.mid" (Save.toByteString (solo 26)) >>
   B.writeFile "haltomato.mid"   (Save.toByteString (song 16 21))
