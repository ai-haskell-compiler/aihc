module Sound.MIDI.Message.Class.Check (
   C(..),
   noteExplicitOff,
   noteImplicitOff,
   controller,
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

import Control.Monad (guard, )


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
   note :: Channel -> event -> Maybe (Velocity, Pitch, Bool)
   program :: Channel -> event -> Maybe Program
   anyController :: Channel -> event -> Maybe (Controller, Int)
   pitchBend :: Channel -> event -> Maybe Int
   channelPressure :: Channel -> event -> Maybe Int
   mode :: Channel -> event -> Maybe Mode.T

   note _chan _ev = Nothing
   program _chan _ev = Nothing
   anyController _chan _ev = Nothing
   pitchBend _chan _ev = Nothing
   channelPressure _chan _ev = Nothing
   mode _chan _ev = Nothing


{- |
Like 'note', but converts @NoteOn p 0@ to @NoteOff p 64@.
See 'VoiceMsg.explicitNoteOff'.
-}
noteExplicitOff ::
   (C event) =>
   Channel -> event -> Maybe (Velocity, Pitch, Bool)
noteExplicitOff chan e =
   fmap CU.explicitNoteOff $ note chan e

{- |
Like 'note', but converts @NoteOff p 64@ to @NoteOn p 0@.
See 'VoiceMsg.implicitNoteOff'.
-}
noteImplicitOff ::
   (C event) =>
   Channel -> event -> Maybe (Velocity, Pitch, Bool)
noteImplicitOff chan e =
   fmap CU.implicitNoteOff $ note chan e


controller ::
   (C event) =>
   Channel -> Controller -> event -> Maybe Int
controller chan ctrl e = do
   (c,n) <- anyController chan e
   guard (ctrl==c)
   return n


lift ::
   (Maybe ChannelMsg.Body -> Maybe a) ->
   Channel -> ChannelMsg.T -> Maybe a
lift act chan msg = do
   guard (ChannelMsg.messageChannel msg  ==  chan)
   act $ Just $ ChannelMsg.messageBody msg

instance C ChannelMsg.T where
   note = lift CU.note
   program = lift CU.program
   anyController = lift CU.anyController
   pitchBend = lift CU.pitchBend
   channelPressure = lift CU.channelPressure
   mode = lift CU.mode


liftMidi ::
   (Channel -> ChannelMsg.T -> Maybe a) ->
   (Channel -> MidiMsg.T -> Maybe a)
liftMidi checkMsg chan msg =
   case msg of
      MidiMsg.Channel chanMsg -> checkMsg chan chanMsg
      _ -> Nothing

instance C MidiMsg.T where
   note = liftMidi note
   program = liftMidi program
   anyController = liftMidi anyController
   pitchBend = liftMidi pitchBend
   channelPressure = liftMidi channelPressure
   mode = liftMidi mode


liftFile ::
   (Channel -> ChannelMsg.T -> Maybe a) ->
   (Channel -> FileEvent.T -> Maybe a)
liftFile checkMsg chan msg =
   case msg of
      FileEvent.MIDIEvent midiMsg -> checkMsg chan midiMsg
      _ -> Nothing

instance C FileEvent.T where
   note = liftFile note
   program = liftFile program
   anyController = liftFile anyController
   pitchBend = liftFile pitchBend
   channelPressure = liftFile channelPressure
   mode = liftFile mode
