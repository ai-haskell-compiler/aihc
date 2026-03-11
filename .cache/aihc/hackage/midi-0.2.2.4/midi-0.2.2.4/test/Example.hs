module Example where

import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event      as Event
import qualified Sound.MIDI.File            as MidiFile

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg

import qualified Data.EventList.Relative.TimeBody as EventList
import Data.EventList.Relative.MixedBody ((/.), (./), )


empty :: MidiFile.T
empty =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 10)
      [EventList.empty]

meta :: MidiFile.T
meta =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 10)
      [EventList.singleton 0 (Event.MetaEvent (MetaEvent.Lyric "foobarz"))]

status :: MidiFile.T
status =
   let chan = ChannelMsg.toChannel 3
       vel  = VoiceMsg.toVelocity 64
   in  MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 10)
          [0 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 20) vel))) ./
           4 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 24) vel))) ./
           4 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 27) vel))) ./
           7 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 20) vel))) ./
           4 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 24) vel))) ./
           4 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 27) vel))) ./
           EventList.empty]


readAfterEnd :: MidiFile.T
readAfterEnd =
   let chan = ChannelMsg.toChannel 3
       vel  = VoiceMsg.toVelocity 64
       pit  = VoiceMsg.toPitch 22
   in  MidiFile.Cons MidiFile.Mixed (MidiFile.Ticks 10)
          [0 /. Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff pit vel))) ./
           EventList.empty]
