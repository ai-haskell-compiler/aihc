--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Queue.Info
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

module Sound.ALSA.Sequencer.Queue.Info
  ( T
  , get
  , set
  , copy
  , clone

  , getQueue
  , getName
  , getLocked
  , getOwner
  , getFlags

  , setName
  , setLocked
  , setOwner
  , setFlags
  ) where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Data.Word (Word, )


#area "queue_info"

#{get_area "Queue.T"}
#{set_area "Queue.T"}


#{get_set_name}
#{get_set_bool "locked", "Locked"}

#{get_set_int "owner", "Owner", "Client.T", "Client.imp", "Client.exp"}
#{get_set_int "flags", "Flags", "Word", "fromIntegral", "fromIntegral"}


-- RO
#{get_int "queue", "Queue", "Queue.T", "Queue.imp"}
