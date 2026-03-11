{- |
MIDI messages in MIDI files.
They are not a superset of the messages,
that are used for real-time communication between MIDI devices.
For these refer to "Sound.MIDI.Message".
Namely System Common and System Real Time messages are missing.
If you need both real-time and file messages (say for ALSA sequencer),
you need a custom datatype.
-}
module Sound.MIDI.File.Event (
   T(..), get, put,
   TrackEvent, getTrackEvent,
   ElapsedTime, fromElapsedTime, toElapsedTime,
   mapBody, maybeMIDIEvent, maybeMetaEvent, maybeVoice, mapVoice,
   ) where

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.File.Event.Meta as MetaEvent

import Sound.MIDI.Message.Channel (Channel)

import Sound.MIDI.File.Event.Meta (
   ElapsedTime, fromElapsedTime, toElapsedTime,
   )

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Status as StatusParser
import qualified Sound.MIDI.Parser.Class  as Parser

import Control.Monad (liftM, liftM2, )

import qualified Sound.MIDI.Writer.Status as StatusWriter
import qualified Sound.MIDI.Writer.Basic  as Writer

import Sound.MIDI.Monoid ((+#+))
import Data.Tuple.HT (mapSnd)


import Test.QuickCheck (Arbitrary(arbitrary), )
import qualified Test.QuickCheck as QC



type TrackEvent = (ElapsedTime, T)

mapBody :: (T -> T) -> (TrackEvent -> TrackEvent)
mapBody = mapSnd


data T =
     MIDIEvent       ChannelMsg.T
   | MetaEvent       MetaEvent.T
   | SystemExclusive SysEx.T
     deriving (Show,Eq,Ord)

instance Arbitrary T where
   arbitrary =
      QC.frequency $
         (100, liftM MIDIEvent arbitrary) :
         (  1, liftM MetaEvent arbitrary) :
         []


maybeMIDIEvent :: T -> Maybe ChannelMsg.T
maybeMIDIEvent (MIDIEvent msg) = Just msg
maybeMIDIEvent _ = Nothing

maybeMetaEvent :: T -> Maybe MetaEvent.T
maybeMetaEvent (MetaEvent mev) = Just mev
maybeMetaEvent _ = Nothing

maybeVoice :: T -> Maybe (Channel, Voice.T)
maybeVoice (MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice ev))) = Just (ch,ev)
maybeVoice _ = Nothing

mapVoice :: (Voice.T -> Voice.T) -> T -> T
mapVoice f (MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice ev))) =
   MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice (f ev)))
mapVoice _ msg = msg


-- * serialization

get :: Parser.C parser => Parser.Fragile (StatusParser.T parser) T
get =
   StatusParser.lift get1 >>= \tag ->
   if tag < 0xF0
     then liftM MIDIEvent $ ChannelMsg.getWithStatus tag
     else
       StatusParser.set Nothing >>
       (StatusParser.lift $
        if tag == 0xFF
          then liftM MetaEvent $ MetaEvent.get
          else liftM SystemExclusive $ SysEx.get tag)

{- |
Each event is preceded by the delta time: the time in ticks between the
last event and the current event.  Parse a time and an event, ignoring
System Exclusive messages.
-}
getTrackEvent :: Parser.C parser => Parser.Fragile (StatusParser.T parser) TrackEvent
getTrackEvent  =  liftM2 (,) (StatusParser.lift getVar) get


{- |
The following functions encode various 'MIDIFile.T' elements
into the raw data of a standard MIDI file.
-}

put ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   T -> StatusWriter.T compress writer
put e =
   case e of
      MIDIEvent       m -> ChannelMsg.putWithStatus m
      MetaEvent       m -> StatusWriter.clear +#+ StatusWriter.lift (MetaEvent.put  m)
      SystemExclusive m -> StatusWriter.clear +#+ StatusWriter.lift (SysEx.put      m)
