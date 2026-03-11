--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Queue.Timer
-- Copyright : (c) Henning Thielemann, 2010-2012
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- This module contains functions for working with sequencer queue.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_queue.html>
--------------------------------------------------------------------------------

module Sound.ALSA.Sequencer.Queue.Timer
  ( T
  , get
  , set
  , copy
  , clone

  , getQueue
  , getType
  , getResolution

  , setType
  , setResolution

  , Type(..)
  ) where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Data.Word (Word, )


#area "queue_timer"

#{get_area "Queue.T"}
#{set_area "Queue.T"}


-- RO
#{get_int "queue", "Queue", "Queue.T", "Queue.imp"}

-- RW

#{get_set_int "type", "Type",
            "Type", "impType", "expType"}

#{get_set_int "resolution", "Resolution",
            "Word", "fromIntegral", "fromIntegral"}


data Type =
     Alsa
   | MidiClock
   | MidiTick
   deriving (Show, Eq, Ord, Enum)

expType :: Type -> C.CInt
expType t  = case t of
  Alsa       -> #{const SND_SEQ_TIMER_ALSA}
  MidiClock  -> #{const SND_SEQ_TIMER_MIDI_CLOCK}
  MidiTick   -> #{const SND_SEQ_TIMER_MIDI_TICK}

impType :: C.CInt -> Type
impType t  = case t of
  #{const SND_SEQ_TIMER_ALSA}         -> Alsa
  #{const SND_SEQ_TIMER_MIDI_CLOCK}   -> MidiClock
  #{const SND_SEQ_TIMER_MIDI_TICK}    -> MidiTick
  _ -> error ("QueueTimer.impType: unknown timer type (" ++ show t ++ ")")
