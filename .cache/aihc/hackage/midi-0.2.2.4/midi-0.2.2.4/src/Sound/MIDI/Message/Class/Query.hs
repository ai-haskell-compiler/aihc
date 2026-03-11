module Sound.MIDI.Message.Class.Query (
   C(..),
   noteExplicitOff,
   noteImplicitOff,
   liftMidi,
   liftFile,
   ) where

import qualified Sound.MIDI.Message.Class.Utility as CU

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Pitch, Velocity, Program, Controller, )

import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.Message as MidiMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import Data.Tuple.HT (mapSnd, )


{- |
All methods have default implementations that return 'Nothing'.
This helps implementing event data types
that support only a subset of types of events.

Maybe a better approach is to provide type classes
for every type of event
and make 'C' a subclass of all of them.
-}
class C event where
   {- |
   Warning: This returns note events as they are,
   that is, a @NoteOff p 64@ might be encoded as such or as @NoteOn p 0@
   depending on the content of @event@.
   For normalized results you may use 'noteExplicitOff'.
   -}
   note :: event -> Maybe (Channel, (Velocity, Pitch, Bool))
   program :: event -> Maybe (Channel, Program)
   anyController :: event -> Maybe (Channel, (Controller, Int))
   pitchBend :: event -> Maybe (Channel, Int)
   channelPressure :: event -> Maybe (Channel, Int)
   mode :: event -> Maybe (Channel, Mode.T)

   note _ev = Nothing
   program _ev = Nothing
   anyController _ev = Nothing
   pitchBend _ev = Nothing
   channelPressure _ev = Nothing
   mode _ev = Nothing


lift ::
   (Maybe ChannelMsg.Body -> Maybe a) ->
   ChannelMsg.T -> Maybe (Channel, a)
lift act msg =
   fmap ((,) (ChannelMsg.messageChannel msg)) $
   act $ Just $ ChannelMsg.messageBody msg

instance C ChannelMsg.T where
   note = lift CU.note
   program = lift CU.program
   anyController = lift CU.anyController
   pitchBend = lift CU.pitchBend
   channelPressure = lift CU.channelPressure
   mode = lift CU.mode

{- |
Like 'note', but converts @NoteOn p 0@ to @NoteOff p 64@.
See 'VoiceMsg.explicitNoteOff'.
-}
noteExplicitOff ::
   (C event) =>
   event -> Maybe (Channel, (Velocity, Pitch, Bool))
noteExplicitOff e =
   fmap (mapSnd CU.explicitNoteOff) $ note e

{- |
Like 'note', but converts @NoteOff p 64@ to @NoteOn p 0@.
See 'VoiceMsg.implicitNoteOff'.
-}
noteImplicitOff ::
   (C event) =>
   event -> Maybe (Channel, (Velocity, Pitch, Bool))
noteImplicitOff e =
   fmap (mapSnd CU.implicitNoteOff) $ note e


liftMidi ::
   (ChannelMsg.T -> Maybe (Channel, a)) ->
   (MidiMsg.T -> Maybe (Channel, a))
liftMidi checkMsg msg =
   case msg of
      MidiMsg.Channel chanMsg -> checkMsg chanMsg
      _ -> Nothing

instance C MidiMsg.T where
   note = liftMidi note
   program = liftMidi program
   anyController = liftMidi anyController
   pitchBend = liftMidi pitchBend
   channelPressure = liftMidi channelPressure
   mode = liftMidi mode


liftFile ::
   (ChannelMsg.T -> Maybe (Channel, a)) ->
   (FileEvent.T -> Maybe (Channel, a))
liftFile checkMsg msg =
   case msg of
      FileEvent.MIDIEvent midiMsg -> checkMsg midiMsg
      _ -> Nothing

instance C FileEvent.T where
   note = liftFile note
   program = liftFile program
   anyController = liftFile anyController
   pitchBend = liftFile pitchBend
   channelPressure = liftFile channelPressure
   mode = liftFile mode
