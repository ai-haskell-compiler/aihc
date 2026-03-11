--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Subscribe
-- Copyright : (c) Henning Thielemann, 2012
--             (c) Dylan Simon, 2011
-- License   : BSD3
--
-- Stability : provisional
--
-- This module contains functions for working with subscriptions.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_subscribe.html>
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Subscribe
  ( T

  , malloc
  , copy
  , clone

  , getSender
  , getDest
  , getQueue
  , getExclusive
  , getTimeUpdate
  , getTimeReal

  , setSender
  , setDest
  , setQueue
  , setExclusive
  , setTimeUpdate
  , setTimeReal

  , subscribePort
  , unsubscribePort

  , create
  , subscribe
  , unsubscribe
  ) where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import Data.Foldable (forM_, )

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )


#area "port_subscribe"

#{get_set_int "queue", "Queue",
           "Queue.T", "Queue.imp", "Queue.exp"}
#get_set_bool "exclusive", "Exclusive"
#get_set_bool "time_update", "TimeUpdate"
#get_set_bool "time_real", "TimeReal"


-- | Get sender address of a port subscription
#{get_ptr "sender", "Sender", "Addr.T"}

-- | Get destination address of a port subscription
#{get_ptr "dest", "Dest", "Addr.T"}

-- | Set sender address of a port subscription
#{set_ptr "sender", "Sender", "Addr.T"}

-- | Set destination address of a port subscription
#{set_ptr "dest", "Dest", "Addr.T"}

-- | Subscribe a port connection
subscribePort :: Seq.T mode -> T -> IO ()
subscribePort (Seq.Cons h) s =
  Exc.checkResult_ "subscribePort" =<< with s (subscribePort_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_subscribe_port"
  subscribePort_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt

-- | Unsubscribe a connection between ports
unsubscribePort :: Seq.T mode -> T -> IO ()
unsubscribePort (Seq.Cons h) s =
  Exc.checkResult_ "unsubscribePort" =<< with s (unsubscribePort_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_unsubscribe_port"
  unsubscribePort_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt

create :: Addr.T -> Addr.T -> Bool -> Maybe (Queue.T, Bool) -> IO T
create sender dest excl time = do
  s <- malloc
  setSender s sender
  setDest s dest
  setExclusive s excl
  forM_ time $ \(queue, realtime) -> do
    setTimeUpdate s True
    setQueue s queue
    setTimeReal s realtime
  return s

-- | Subscribe a port connection: @'subscribeSimple' sender dest exclusive (Just (updatequeue, realtime))@
subscribe :: Seq.T mode -> Addr.T -> Addr.T -> Bool -> Maybe (Queue.T, Bool) -> IO ()
subscribe ss sender dest excl time =
  subscribePort ss =<< create sender dest excl time

-- | Unsubscribe a port connection: @'unsubscribeSimple' sender dest@
unsubscribe :: Seq.T mode -> Addr.T -> Addr.T -> IO ()
unsubscribe ss sender dest =
  unsubscribePort ss =<< create sender dest False Nothing
