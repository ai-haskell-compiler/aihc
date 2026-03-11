--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Queue.Tempo
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

module Sound.ALSA.Sequencer.Queue.Tempo
  ( T
  , get
  , set
  , copy
  , clone

  , getQueue
  , getTempo
  , getPPQ
  , getSkew
  , getSkewBase

  , setTempo
  , setPPQ
  , setSkew
  , setSkewBase
  ) where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Data.Word (Word, )


#area "queue_tempo"

#{get_area "Queue.T"}
#{set_area "Queue.T"}


-- RO
#{get_int "queue", "Queue", "Queue.T", "Queue.imp"}

-- RW
#{get_set_int "tempo", "Tempo", "Word", "fromIntegral", "fromIntegral"}
#{get_set_int "ppq", "PPQ", "Int", "fromIntegral", "fromIntegral"}
#{get_set_int "skew", "Skew", "Word", "fromIntegral", "fromIntegral"}
#{get_set_int "skew_base", "SkewBase", "Word", "fromIntegral", "fromIntegral"}
