--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Client
-- Copyright : (c) Henning Thielemann, 2010-2012
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- This module contains functions for working with sequencer clients.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_client.html>
--------------------------------------------------------------------------------

module Sound.ALSA.Sequencer.Client.Info
  ( T

  , get
  , getAny
  , queryFirst
  , queryNext
  , queryLoop_
  , queryLoop
  , set

  , copy
  , clone

  , getClient
  , getType
  , getName
  , getBroadcastFilter
  , getErrorBounce
  , getNumPorts
  , getEventLost

  , setClient
  , setName
  , setBroadcastFilter
  , setErrorBounce

  , filterClear
  , filterAdd
  , filterDel
  , filterCheck
  ) where

import Sound.ALSA.Sequencer.Marshal.ClientInfo

import qualified Sound.ALSA.Sequencer.Client.Info.EventFilter as Filter

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event
import qualified Sound.ALSA.Sequencer.Query as Query


queryFirst :: Seq.T mode -> IO T
queryFirst = Query.first

queryLoop_ :: Seq.T mode -> (T -> IO ()) -> IO ()
queryLoop_ h = Query.loop_ h (const $ return ())

queryLoop :: Seq.T mode -> (T -> IO a) -> IO [a]
queryLoop h = Query.loop h (const $ return ())


filterClear :: T -> IO ()
filterAdd :: Event.Type e => T -> e -> IO ()
filterDel :: Event.Type e => T -> e -> IO ()
filterCheck :: Event.Type e => T -> e -> IO Bool

filterClear = Filter.clear
filterAdd   = Filter.add
filterDel   = Filter.delete
filterCheck = Filter.check
