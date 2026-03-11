--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Subscribe.Query
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
module Sound.ALSA.Sequencer.Subscribe.Query
  ( T
  , Type(..)

  , malloc
  , copy
  , clone

  , getClient
  , getPort
  , getRoot
  , getType
  , getIndex
  , getNumSubs
  , getAddr
  , getQueue
  , getExclusive
  , getTimeUpdate
  , getTimeReal

  , setClient
  , setPort
  , setType
  , setIndex

  , query
  , queryAll
  ) where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Port as Port
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )
import Data.Word (Word, )
import Data.Maybe.HT (toMaybe, )


#area "query_subscribe"

#{get_set_int "client", "Client",
          "Client.T", "Client.imp", "Client.exp"}
#{get_set_int "port", "Port",
          "Port.T", "Port.imp", "Port.exp"}
#{get_set_int "type", "Type",
          "Type", "impType", "expType"}
#{get_set_int "index", "Index",
          "Word", "fromIntegral", "fromIntegral"}

-- RO
#{get_int "num_subs", "NumSubs", "Word", "fromIntegral"}
#{get_int "queue", "Queue", "Queue.T", "Queue.imp"}
#get_bool "exclusive", "Exclusive"
#get_bool "time_update", "TimeUpdate"
#get_bool "time_real", "TimeReal"


-- | Get the client/port address of a query
#{get_ptr "root", "Root", "Addr.T"}
-- | Set the client/port address of a query
#{set_ptr "root", "Root", "Addr.T"}
-- | Get the address of subscriber of query
#{get_ptr "addr", "Addr", "Addr.T"}


-- | Query port subscriber list
queryPort :: Seq.T mode -> T -> IO Bool
queryPort (Seq.Cons h) q =
  U.checkResultQuery "Subscribe.queryPort" =<< with q (queryPort_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_query_port_subscribers"
  queryPort_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt

-- | Queries a subscriber connected to (Write) or from (Read) a given address: @'query' seq addr typ index@
query :: Seq.T mode -> Addr.T -> Type -> Word -> IO (Maybe T)
query ss root t i = do
  q <- malloc
  setRoot q root
  setType q t
  setIndex q i
  r <- queryPort ss q
  return $ toMaybe r q

-- | Queries the list of subscribers accessing a port
queryAll :: Seq.T mode -> Addr.T -> Type -> IO [T]
queryAll ss root t = queryRest 0 where
  queryRest i = query ss root t i >>=
    maybe (return []) (\q -> (q:) `fmap` queryRest (succ i))


data Type =
     Read
   | Write
   deriving (Show, Eq, Ord, Enum)

expType :: Type -> C.CInt
expType t  = case t of
  Read  -> #{const SND_SEQ_QUERY_SUBS_READ}
  Write -> #{const SND_SEQ_QUERY_SUBS_WRITE}

impType :: C.CInt -> Type
impType t  = case t of
  #{const SND_SEQ_QUERY_SUBS_READ}  -> Read
  #{const SND_SEQ_QUERY_SUBS_WRITE} -> Write
  _ -> error ("QuerySubscribe.impType: unknown subscription type (" ++ show t ++ ")")
