--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal.PortInfo
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

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Marshal.PortInfo where

#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Port as Port
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Sequencer.Query as Query
import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Sound.ALSA.Exception as Exc

import qualified Data.EnumBitSet as EnumSet

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )
import Data.Word (Word, )


#area "port_info"

{- |
Create a new information area filled with data
about a specific port on our client.
-}
#get_area "Port.T"
{- |
Set the information for the sequencer port
based on the data in the given information area.
-}
#set_area "Port.T"

-- read/write
#get_set_name

#get_set_bool "port_specified", "PortSpecified"
#get_set_bool "timestamping", "Timestamping"
#get_set_bool "timestamp_real", "TimestampReal"

#{get_set_int "port", "Port",
          "Port.T", "Port.imp", "Port.exp"}
#{get_set_int "client", "Client",
          "Client.T", "Client.imp", "Client.exp"}
#{get_set_int "capability", "Capability",
          "Port.Cap", "(EnumSet.Cons . fromIntegral)", "(fromIntegral . EnumSet.decons)"}

#{get_set_int "midi_channels", "MidiChannels",
          "Word", "fromIntegral", "fromIntegral"}
#{get_set_int "midi_voices", "MidiVoices",
          "Word", "fromIntegral", "fromIntegral"}
#{get_set_int "synth_voices", "SynthVoices",
          "Word", "fromIntegral", "fromIntegral"}

#{get_set_int "timestamp_queue", "TimestampQueue",
          "Queue.T", "Queue.imp", "Queue.exp"}


-- | Get the address of the information area.
#{get_ptr "addr", "Addr", "Addr.T"}

-- | Set the port address.
#{set_ptr "addr", "Addr", "Addr.T"}


-- read only
#{get_int "read_use", "ReadUse", "Word", "fromIntegral"}
#{get_int "write_use", "WriteUse", "Word", "fromIntegral"}



-- | Create a new information area filled with data about an given
-- port on a given client.
getAny :: Seq.T mode -> Client.T -> Port.T -> IO T
getAny (Seq.Cons h) c p =
  do info <- malloc
     Exc.checkResult_ "getAny" =<<
       with info (getAny_ h (Client.exp c) (Port.exp p))
     return info

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_get_any_port_info"
  getAny_
    :: Ptr Seq.Core -> C.CInt -> C.CInt -> Ptr T_ -> IO C.CInt

queryInit :: T -> IO ()
queryInit x =
  {-
  we cannot use setPort here,
  since Port uses an unsigned type and thus cannot represent -1
  -}
  with x (flip setPort_ (-1))

-- | Get information about the first port on our client.
queryFirst :: Seq.T mode -> IO T
queryFirst = Query.first

-- | Get information about the port with the next biggest identifier.
-- If a matching port is found, then its information is stored in the
-- given area and 'True' is returned.
queryNext :: Seq.T mode -> T -> IO Bool
queryNext (Seq.Cons h) info =
  U.checkResultQuery "PortInfo.queryNext" =<< with info (queryNext_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_query_next_port"
  queryNext_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt


instance Query.C T where
  init = queryInit
  next = queryNext
