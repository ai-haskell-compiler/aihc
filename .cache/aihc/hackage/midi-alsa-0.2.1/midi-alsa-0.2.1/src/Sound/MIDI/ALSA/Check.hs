{-
The instances Class.C Event.T and Class.C Event.Data are orphan.
I could put it in package 'midi' or 'alsa-seq'
but in both of them it imposes a lot of new dependencies.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sound.MIDI.ALSA.Check (
   Class.C,
   Class.note,
   Class.noteExplicitOff,
   Class.noteImplicitOff,
   Class.program,
   Class.anyController,
   Class.controller,
   Class.mode,
   Class.pitchBend,
   Class.channelPressure,
   ) where

import qualified Sound.MIDI.Message.Class.Check as Class
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import qualified Sound.MIDI.ALSA as MALSA

import qualified Sound.ALSA.Sequencer.Event as Event

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Pitch, Velocity, Controller, Program, )

import Data.Accessor.Basic ((^.), )
import Data.Maybe.HT (toMaybe, )
import Control.Monad (guard, )


instance Class.C Event.T where
   note chan = noteData chan . Event.body
   program chan = programData chan . Event.body
   anyController chan = anyControllerData chan . Event.body
   mode chan = modeData chan . Event.body
   pitchBend chan = pitchBendData chan . Event.body
   channelPressure chan = channelPressureData chan . Event.body

instance Class.C Event.Data where
   note = noteData
   program = programData
   anyController = anyControllerData
   mode = modeData
   pitchBend = pitchBendData
   channelPressure = channelPressureData


noteData ::
   Channel -> Event.Data -> Maybe (Velocity, Pitch, Bool)
noteData chan e = do
   Event.NoteEv n c <- Just e
   guard (c ^. MALSA.noteChannel  ==  chan)
   let pitch    = c ^. MALSA.notePitch
       velocity = c ^. MALSA.noteVelocity
   case n of
      Event.NoteOn  -> Just (velocity, pitch, True)
      Event.NoteOff -> Just (velocity, pitch, False)
      _ -> Nothing

programData ::
   Channel -> Event.Data -> Maybe Program
programData chan e = do
   Event.CtrlEv Event.PgmChange c <- Just e
   guard (c ^. MALSA.ctrlChannel  ==  chan)
   return $ c ^. MALSA.ctrlProgram

anyControllerData ::
   Channel -> Event.Data -> Maybe (Controller, Int)
anyControllerData chan e = do
   -- let Event.TickTime n = Event.timestamp e
   Event.CtrlEv Event.Controller c <- Just e
   guard (c ^. MALSA.ctrlChannel  ==  chan)
   MALSA.Controller cn cv <- Just $ c ^. MALSA.ctrlControllerMode
   return (cn, cv)

modeData :: Channel -> Event.Data -> Maybe Mode.T
modeData chan e = do
   Event.CtrlEv Event.Controller c <- Just e
   guard (c ^. MALSA.ctrlChannel  ==  chan)
   MALSA.Mode m <- Just $ c ^. MALSA.ctrlControllerMode
   return m

pitchBendData :: Channel -> Event.Data -> Maybe Int
pitchBendData chan e =
   case e of
      Event.CtrlEv Event.PitchBend c ->
         toMaybe
            (c ^. MALSA.ctrlChannel == chan)
            (c ^. MALSA.ctrlValue)
      _ -> Nothing

channelPressureData :: Channel -> Event.Data -> Maybe Int
channelPressureData chan e =
   case e of
      Event.CtrlEv Event.ChanPress c ->
         toMaybe
            (c ^. MALSA.ctrlChannel == chan)
            (c ^. MALSA.ctrlValue)
      _ -> Nothing
