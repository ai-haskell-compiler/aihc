--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Port.Info
-- Copyright : (c) Henning Thielemann, 2010-2012
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- This module contains functions for working with ports.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_port.html>
--------------------------------------------------------------------------------

module Sound.ALSA.Sequencer.Port.Info
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

  , getPort
  , getClient
  , getAddr
  , getName
  , getCapability
  , getMidiChannels
  , getMidiVoices
  , getSynthVoices
  , getPortSpecified
  , getTimestamping
  , getTimestampReal
  , getTimestampQueue

  , getReadUse
  , getWriteUse

  , setPort
  , setClient
  , setAddr
  , setName
  , setCapability
  , setMidiChannels
  , setSynthVoices
  , setMidiVoices
  , setPortSpecified
  , setTimestamping
  , setTimestampReal
  , setTimestampQueue
  ) where

import Sound.ALSA.Sequencer.Marshal.PortInfo

import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq

import qualified Sound.ALSA.Sequencer.Query as Query


queryLoop_ :: Seq.T mode -> Client.T -> (T -> IO ()) -> IO ()
queryLoop_ h c = Query.loop_ h (flip setClient c)

queryLoop :: Seq.T mode -> Client.T -> (T -> IO a) -> IO [a]
queryLoop h c = Query.loop h (flip setClient c)
