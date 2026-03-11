--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Queue
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

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Queue
  ( -- * General Queue Functions
    Queue.T
  , Queue.direct
  , alloc
  , allocNamed
  , free
  , with
  , withNamed
  , control

  ) where

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.C.String (CString, withCAString, )
import Foreign.Ptr (Ptr, )

import Control.Exception (bracket, )
import Control.Functor.HT (void, )


alloc :: Seq.T mode -> IO Queue.T -- ^ Queue.T identifier.
alloc (Seq.Cons h) =
  fmap Queue.imp $ Exc.checkResult "Queue.alloc" =<< alloc_ h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_alloc_queue"
  alloc_ :: Ptr Seq.Core -> IO C.CInt

with :: Seq.T mode -> (Queue.T -> IO a) -> IO a
with s = bracket (alloc s) (free s)


allocNamed :: Seq.T mode -> String -> IO Queue.T
allocNamed (Seq.Cons h) x = withCAString x $ \s ->
  fmap Queue.imp $ Exc.checkResult "Queue.allocNamed" =<< allocNamed_ h s

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_alloc_named_queue"
  allocNamed_ :: Ptr Seq.Core -> CString -> IO C.CInt

withNamed :: Seq.T mode -> String -> (Queue.T -> IO a) -> IO a
withNamed s nm = bracket (allocNamed s nm) (free s)


-- | Delete the specified queue.
free
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Queue.T    -- ^ Queue.T identifier.
  -> IO ()
free (Seq.Cons h) q =
  Exc.checkResult_ "Queue.free" =<< free_ h (Queue.exp q)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_free_queue"
  free_ :: Ptr Seq.Core -> C.CInt -> IO C.CInt


{- |
start/stop/continue a queue

In the prototype event you can provide additional information.
The prototype event does not need to be a queue control event,
this part is ignored anyway.
In the prototype event you may also specify a queue.
This is the queue that the timestamp of the prototype event refers to.
This way you can control the target queue using timing from another queue.
-}
control
  :: Seq.T mode    -- ^ Sequencer handle.
  -> Queue.T       -- ^ target Queue.T.
  -> Event.QueueEv
  -> Maybe Event.T -- ^ prototype event that may provide timestamp, source queue
  -> IO ()
control h q cmd me =
  case cmd of
    Event.QueueSetPosTick _ -> controlCustom h q cmd me
    Event.QueueSetPosTime _ -> controlCustom h q cmd me
    Event.QueueSkew _       -> controlCustom h q cmd me
    _ -> controlPlain h q cmd me

controlCustom, controlPlain ::
  Seq.T mode -> Queue.T -> Event.QueueEv -> Maybe Event.T -> IO ()
controlCustom h q cmd me =
  eventOutput h $
    case me of
      Nothing ->
        -- cf. Event.simple
        Event.Cons {
          Event.highPriority = False,
          Event.tag = Event.Tag 0,
          Event.queue = Queue.direct,
          Event.time = Time.consAbs $ Time.Tick 0,
          Event.source = Addr.unknown,
          Event.dest = Addr.systemTimer,
          Event.body = Event.QueueEv cmd q
        }
      Just ev ->
        ev {
          Event.dest = Addr.systemTimer,
          Event.body = Event.QueueEv cmd q
        }

eventOutput :: Seq.T mode -> Event.T -> IO ()
eventOutput h e =
  void $
  Event.with e $ \p ->
     Exc.checkResult "Queue.control.eventOutput" =<< eventOutput_ h p

foreign import ccall safe "alsa/asoundlib.h snd_seq_event_output"
  eventOutput_ :: Seq.T mode -> Ptr Event.T -> IO C.CInt

controlPlain h q cmd me =
  Event.withMaybe me $ \p ->
    let (c,v) = Event.expQueueEv cmd
    in  Exc.checkResult_ "Queue.control"
          =<< control_ h (Queue.exp q)
                 (fromIntegral $ Event.unEType c) v p

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_control_queue"
  control_ :: Seq.T mode -> C.CInt -> C.CInt -> C.CInt -> Ptr Event.T -> IO C.CInt
