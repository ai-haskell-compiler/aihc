module Sound.MIDI.Message.Class.Utility where

import Sound.MIDI.Message.Channel.Voice (Pitch, Velocity, Program, Controller, )

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel as ChannelMsg


note :: Maybe ChannelMsg.Body -> Maybe (Velocity, Pitch, Bool)
program :: Maybe ChannelMsg.Body -> Maybe Program
anyController :: Maybe ChannelMsg.Body -> Maybe (Controller, Int)
pitchBend :: Maybe ChannelMsg.Body -> Maybe Int
channelPressure :: Maybe ChannelMsg.Body -> Maybe Int
mode :: Maybe ChannelMsg.Body -> Maybe Mode.T

note msg = do
   ChannelMsg.Voice voice <- msg
   case voice of
      VoiceMsg.NoteOn  pitch velocity -> Just (velocity, pitch, True)
      VoiceMsg.NoteOff pitch velocity -> Just (velocity, pitch, False)
      _ -> Nothing

program msg = do
   ChannelMsg.Voice (VoiceMsg.ProgramChange pgm) <- msg
   return pgm

anyController msg = do
   ChannelMsg.Voice (VoiceMsg.Control ctrl val) <- msg
   return (ctrl, val)

pitchBend msg = do
   ChannelMsg.Voice (VoiceMsg.PitchBend bend) <- msg
   return bend

channelPressure msg = do
   ChannelMsg.Voice (VoiceMsg.MonoAftertouch pressure) <- msg
   return pressure

mode msg = do
   ChannelMsg.Mode m <- msg
   return m


explicitNoteOff ::
   (Velocity, Pitch, Bool) -> (Velocity, Pitch, Bool)
explicitNoteOff x@(v,p,b) =
   if b && v == VoiceMsg.toVelocity 0
     then (VoiceMsg.toVelocity 64, p, False)
     else x

implicitNoteOff ::
   (Velocity, Pitch, Bool) -> (Velocity, Pitch, Bool)
implicitNoteOff x@(v,p,b) =
   if not b && v == VoiceMsg.toVelocity 64
     then (VoiceMsg.toVelocity 0, p, True)
     else x
