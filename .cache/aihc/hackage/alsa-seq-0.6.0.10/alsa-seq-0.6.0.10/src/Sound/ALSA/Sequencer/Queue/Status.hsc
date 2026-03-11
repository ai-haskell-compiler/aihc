--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Queue.Status
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

module Sound.ALSA.Sequencer.Queue.Status
  ( T
  , get
  , copy
  , clone

  , getQueue
  , getEvents
  , getTickTime
  , getRealTime
  ) where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C


#area "queue_status"


#{get_area "Queue.T"}

#{get_int "tick_time", "TickTime", "Time.Tick", "fromIntegral"}
#{get_ptr "real_time", "RealTime", "RealTime.T"}
#{get_int "queue", "Queue", "Queue.T", "Queue.imp"}
#{get_int "events", "Events", "Int", "fromIntegral"}

{-
This function shall return status bits of the queue,
but the ALSA headers do not define any bits.
I would prefer a data type that handles this bitfield.

#{get_int "status", "Status", "C.CUInt", "fromIntegral"}
-}
