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

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Marshal.ClientInfo where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Sequencer.Query as Query
import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )
import Data.Word (Word, )


#area "client_info"

-- read/write
#get_set_name
#get_set_bool "broadcast_filter", "BroadcastFilter"
#get_set_bool "error_bounce", "ErrorBounce"
#{get_set_int "client", "Client",
          "Client.T", "Client.imp", "Client.exp"}

-- read only
#{get_int "type", "Type",
          "Client.Type", "Client.impType"}
#{get_int "num_ports", "NumPorts",
          "Word", "fromIntegral"}
#{get_int "event_lost", "EventLost",
          "Word", "fromIntegral"}



-- | Create a new information area filled with data about the sequencer client.
get :: Seq.T mode -> IO T
get (Seq.Cons h) =
  do info <- malloc
     Exc.checkResult_ "ClientInfo.get" =<< with info (get_ h)
     return info

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_get_client_info"
  get_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt


-- | Create a new information area filled with data about an arbitrary client.
getAny :: Seq.T mode -> Client.T -> IO T
getAny (Seq.Cons h) c =
  do info <- malloc
     Exc.checkResult_ "ClientInfo.getAny" =<<
       with info (getAny_ h (Client.exp c))
     return info

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_get_any_client_info"
  getAny_
    :: Ptr Seq.Core -> C.CInt -> Ptr T_ -> IO C.CInt


-- | Set the information for the sequencer client based on the data
-- in the given information area.
set :: Seq.T mode -> T -> IO ()
set (Seq.Cons h) info =
  Exc.checkResult_ "set_client_info" =<< with info (set_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_client_info"
  set_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt



queryInit :: T -> IO ()
queryInit x =
  {-
  we cannot use setClient here,
  since Client uses an unsigned type and thus cannot represent -1
  -}
  with x (flip setClient_ (-1))

-- | Get information about the client with the next biggest identifier.
queryNext :: Seq.T mode -> T -> IO Bool  -- ^ Was there a next client?
queryNext (Seq.Cons h) info =
  U.checkResultQuery "ClientInfo.queryNext" =<< with info (queryNext_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_query_next_client"
  queryNext_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt

instance Query.C T where
  init = queryInit
  next = queryNext
