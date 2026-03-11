module Sound.MIDI.Message.Class.Construct where

import qualified Sound.MIDI.Message.Class.Utility as CU

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Pitch, Velocity, Program, Controller, )

import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.Message as MidiMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode


class C event where
   {- |
   Warning: This constructs a note events as is,
   that is, a @NoteOff p 64@ is encoded as such
   and will not be converted to @NoteOn p 0@.
   If you want such a conversion, you may use 'noteImplicitOff'.
   -}
   note :: Channel -> (Velocity, Pitch, Bool) -> event
   program :: Channel -> Program -> event
   anyController :: Channel -> (Controller, Int) -> event
   pitchBend :: Channel -> Int -> event
   channelPressure :: Channel -> Int -> event
   mode :: Channel -> Mode.T -> event


liftChannel ::
   (a -> ChannelMsg.Body) ->
   (Channel -> a -> ChannelMsg.T)
liftChannel makeMsg channel param =
   ChannelMsg.Cons channel $ makeMsg param

instance C ChannelMsg.T where
   note =
      liftChannel $ \(velocity, pitch, on) ->
         ChannelMsg.Voice $
         (if on then VoiceMsg.NoteOn else VoiceMsg.NoteOff) pitch velocity

   program =
      liftChannel $ \pgm ->
         ChannelMsg.Voice $ VoiceMsg.ProgramChange pgm

   anyController =
      liftChannel $ \(ctrl, val) ->
         ChannelMsg.Voice $ VoiceMsg.Control ctrl val

   pitchBend =
      liftChannel $ \bend ->
         ChannelMsg.Voice $ VoiceMsg.PitchBend bend

   channelPressure =
      liftChannel $ \pressure ->
         ChannelMsg.Voice $ VoiceMsg.MonoAftertouch pressure

   mode =
      liftChannel $ \m ->
         ChannelMsg.Mode m


{- |
Like 'note', but converts @NoteOn p 0@ to @NoteOff p 64@.
See 'VoiceMsg.explicitNoteOff'.
-}
noteExplicitOff ::
   (C event) =>
   Channel -> (Velocity, Pitch, Bool) -> event
noteExplicitOff channel =
   note channel . CU.explicitNoteOff

{- |
Like 'note', but converts @NoteOff p 64@ to @NoteOn p 0@.
See 'VoiceMsg.implicitNoteOff'.
-}
noteImplicitOff ::
   (C event) =>
   Channel -> (Velocity, Pitch, Bool) -> event
noteImplicitOff channel =
   note channel . CU.implicitNoteOff


liftMidi ::
   (Channel -> a -> ChannelMsg.T) ->
   (Channel -> a -> MidiMsg.T)
liftMidi makeMsg channel msg =
   MidiMsg.Channel $ makeMsg channel msg

instance C MidiMsg.T where
   note = liftMidi note
   program = liftMidi program
   anyController = liftMidi anyController
   pitchBend = liftMidi pitchBend
   channelPressure = liftMidi channelPressure
   mode = liftMidi mode


liftFile ::
   (Channel -> a -> ChannelMsg.T) ->
   (Channel -> a -> FileEvent.T)
liftFile makeMsg channel msg =
   FileEvent.MIDIEvent $ makeMsg channel msg

instance C FileEvent.T where
   note = liftFile note
   program = liftFile program
   anyController = liftFile anyController
   pitchBend = liftFile pitchBend
   channelPressure = liftFile channelPressure
   mode = liftFile mode
