module Sound.MIDI.Example.ControllerRamp {- Main -} where

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Save as Save

import qualified Sound.MIDI.File.Event      as Event

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Data.ByteString.Lazy as B



example :: MidiFile.T
example =
   let chan = ChannelMsg.toChannel 0
   in  MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 10)
          [EventList.fromPairList $
           map (\x -> (50, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.Control VoiceMsg.mainVolume x)))))
               [0..127]]

main :: IO ()
main =
   B.writeFile "controller-ramp.mid"
      (Save.toByteString example)
