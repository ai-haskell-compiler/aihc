{-
The instances Class.C Event.T and Class.C Event.Data are orphan.
I could put them in package 'midi' or 'alsa-seq'
but in both of them it imposes a lot of new dependencies.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sound.MIDI.ALSA.Query (
   Class.C,
   Class.note,
   Class.noteExplicitOff,
   Class.noteImplicitOff,
   Class.program,
   Class.anyController,
   Class.mode,
   Class.pitchBend,
   Class.channelPressure,
   ) where

import qualified Sound.MIDI.Message.Class.Query as Class
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import qualified Sound.MIDI.ALSA as MALSA

import qualified Sound.ALSA.Sequencer.Event as Event

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Pitch, Velocity, Controller, Program, )

import Data.Accessor.Basic ((^.), )


instance Class.C Event.T where
   note = noteData . Event.body
   program = programData . Event.body
   anyController = anyControllerData . Event.body
   mode = modeData . Event.body
   pitchBend = pitchBendData . Event.body
   channelPressure = channelPressureData . Event.body

instance Class.C Event.Data where
   note = noteData
   program = programData
   anyController = anyControllerData
   mode = modeData
   pitchBend = pitchBendData
   channelPressure = channelPressureData


noteData ::
   Event.Data -> Maybe (Channel, (Velocity, Pitch, Bool))
noteData e = do
   Event.NoteEv n c <- Just e
   let pitch    = c ^. MALSA.notePitch
       velocity = c ^. MALSA.noteVelocity
   fmap ((,) (c ^. MALSA.noteChannel)) $
      case n of
         Event.NoteOn  -> Just (velocity, pitch, True)
         Event.NoteOff -> Just (velocity, pitch, False)
         _ -> Nothing

programData ::
   Event.Data -> Maybe (Channel, Program)
programData e = do
   Event.CtrlEv Event.PgmChange c <- Just e
   return (c ^. MALSA.ctrlChannel, c ^. MALSA.ctrlProgram)

anyControllerData ::
   Event.Data -> Maybe (Channel, (Controller, Int))
anyControllerData e = do
   -- let Event.TickTime n = Event.timestamp e
   Event.CtrlEv Event.Controller c <- Just e
   MALSA.Controller cn cv <- Just $ c ^. MALSA.ctrlControllerMode
   return (c ^. MALSA.ctrlChannel, (cn, cv))

modeData :: Event.Data -> Maybe (Channel, Mode.T)
modeData e = do
   Event.CtrlEv Event.Controller c <- Just e
   MALSA.Mode m <- Just $ c ^. MALSA.ctrlControllerMode
   return (c ^. MALSA.ctrlChannel, m)

pitchBendData :: Event.Data -> Maybe (Channel, Int)
pitchBendData e =
   case e of
      Event.CtrlEv Event.PitchBend c ->
         Just (c ^. MALSA.ctrlChannel, c ^. MALSA.ctrlValue)
      _ -> Nothing

channelPressureData :: Event.Data -> Maybe (Channel, Int)
channelPressureData e =
   case e of
      Event.CtrlEv Event.ChanPress c ->
         Just (c ^. MALSA.ctrlChannel, c ^. MALSA.ctrlValue)
      _ -> Nothing
