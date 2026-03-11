{-
The instances Class.C Event.T and Class.C Event.Data are orphan.
I could put them in package 'midi' or 'alsa-seq'
but in both of them it imposes a lot of new dependencies.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sound.MIDI.ALSA.Construct (
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

import qualified Sound.MIDI.Message.Class.Construct as Class
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import qualified Sound.MIDI.ALSA as MALSA

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Address as Addr

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice
          (Pitch, Velocity, Controller, Program, toVelocity, )


liftEvent ::
   (Channel -> a -> Event.Data) ->
   (Channel -> a -> Event.T)
liftEvent makeEvent chan param =
   Event.simple Addr.unknown $ makeEvent chan param

instance Class.C Event.T where
   note = liftEvent noteData
   program = liftEvent programData
   anyController = liftEvent anyControllerData
   mode = liftEvent modeData
   pitchBend = liftEvent pitchBendData
   channelPressure = liftEvent channelPressureData

instance Class.C Event.Data where
   note = noteData
   program = programData
   anyController = anyControllerData
   mode = modeData
   pitchBend = pitchBendData
   channelPressure = channelPressureData


noteData ::
   Channel -> (Velocity, Pitch, Bool) -> Event.Data
noteData chan (velocity, pitch, on) =
   Event.NoteEv
      (if on then Event.NoteOn else Event.NoteOff)
      (MALSA.noteEvent chan pitch velocity (toVelocity 0) 0)

programData ::
   Channel -> Program -> Event.Data
programData chan pgm =
   Event.CtrlEv Event.PgmChange $ MALSA.programChangeEvent chan pgm

anyControllerData ::
   Channel -> (Controller, Int) -> Event.Data
anyControllerData chan (ctrl, val) =
   Event.CtrlEv Event.Controller $ MALSA.controllerEvent chan ctrl val

modeData :: Channel -> Mode.T -> Event.Data
modeData chan mode =
   Event.CtrlEv Event.Controller $ MALSA.modeEvent chan mode

pitchBendData :: Channel -> Int -> Event.Data
pitchBendData chan val =
   Event.CtrlEv Event.PitchBend $ MALSA.controllerEvent chan minBound val

channelPressureData :: Channel -> Int -> Event.Data
channelPressureData chan val =
   Event.CtrlEv Event.ChanPress $ MALSA.controllerEvent chan minBound val
