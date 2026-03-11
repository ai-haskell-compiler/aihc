{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.ALSA.Dimensional.Server.Test where

import Synthesizer.ALSA.Dimensional.Server.Common
import Synthesizer.MIDI.Dimensional.Example.Instrument (ping, )

import qualified Synthesizer.ALSA.Storable.Play as PlaySt

import qualified Synthesizer.MIDI.Dimensional as MIDI
import qualified Synthesizer.MIDI.EventList as MIDIEv
import qualified Synthesizer.MIDI.Generic as MidiG

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc

import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (break, )



sequence1 :: IO ()
sequence1 =
--   print =<<
--   File.renderTimeVoltageMonoDoubleToInt16 sampleRate "test.wav"
   SVL.writeFile "test.f32"
      (SigA.scalarSamples (DN.toNumberWithDimension Dim.voltage)
      (SigA.render sampleRate
         (let evs t = EventList.cons t ([]::[Event.T]) (evs (20-t))
              {-
              evs0 =
                 EventList.cons 10 [makeNote AlsaMidi.NoteOn 60] $
                 EventList.cons 10 [makeNote AlsaMidi.NoteOn 64] $
                 evs 10
              -}
          in  MIDI.runFilter (evs 10)
                 (MIDI.sequence PlaySt.defaultChunkSize
                    (DN.voltage 1) channel ping))))

sequence2 ::
   EventList.T MIDIEv.StrictTime [SigSt.T Real]
sequence2 =
   fmap (map SigA.body) $
   flip Proc.process sampleRate
      (let evs t = EventList.cons t ([]::[Event.T]) (evs (20-t))
       in  MIDI.runFilter (evs 10)
              (MIDI.prepareTones channel MidiG.errorNoProgram
                 (const ping)))
