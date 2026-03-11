--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Client
-- Copyright : (c) Henning Thielemann, 2010
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

module Sound.ALSA.Sequencer.Client
  ( Client.T(Client.Cons)
  , Client.unknown
  , Client.system
  , Client.subscribers
  , Client.broadcast

  , getId
  , setName

  , Client.Type(..)
  ) where

import qualified Sound.ALSA.Sequencer.Client.InfoMonad as ClientInfo

import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq


-- XXX: Still missing the pool interface.

-- Convinience functions -------------------------------------------------------
-- These are part of the "middle" interface, but it seems simple to
-- define them directly in Haskell.

-- | Get the client identifier for the sequencer.
-- A convinience function.
getId :: Seq.T mode -> IO Client.T
getId h = ClientInfo.get h ClientInfo.getClient

-- | Set the name for the sequencer client.
-- A convinience function.
setName :: Seq.T mode -> String -> IO ()
setName h s = ClientInfo.modify h $ ClientInfo.setName s
