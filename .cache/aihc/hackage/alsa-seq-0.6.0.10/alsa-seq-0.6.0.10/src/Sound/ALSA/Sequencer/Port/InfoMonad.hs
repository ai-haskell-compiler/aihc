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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Port.InfoMonad (
  T,

  run,
  get,
  modify,

  getPort,
  getClient,
  getAddr,
  getName,
  getCapability,
  getMidiChannels,
  getMidiVoices,
  getSynthVoices,
  getPortSpecified,
  getTimestamping,
  getTimestampReal,
  getTimestampQueue,

  getReadUse,
  getWriteUse,

  setPort,
  setClient,
  setAddr,
  setName,
  setCapability,
  setMidiChannels,
  setSynthVoices,
  setMidiVoices,
  setPortSpecified,
  setTimestamping,
  setTimestampReal,
  setTimestampQueue,
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.PortInfo as PortInfo
import qualified Sound.ALSA.Sequencer.Marshal.Port as Port
import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq

import qualified Control.Monad.Trans.Reader as MR
import Control.Exception (bracket)
import Control.Applicative (Applicative, )

import Data.Word (Word, )


newtype T a = Cons (MR.ReaderT PortInfo.T IO a)
   deriving (Functor, Applicative, Monad)


run :: T a -> PortInfo.T -> IO a
run (Cons m) = MR.runReaderT m

get :: Seq.T mode -> Port.T -> T a -> IO a
get h p m = run m =<< PortInfo.get h p

modify :: Seq.T mode -> Port.T -> T a -> IO a
modify h p m =
   bracket (PortInfo.get h p) (PortInfo.set h p) (run m)


liftGet :: (PortInfo.T -> IO a) -> T a
liftGet f = Cons $ MR.ReaderT f

liftSet :: (PortInfo.T -> b -> IO a) -> b -> T a
liftSet f x = Cons $ MR.ReaderT $ flip f x


getPort :: T Port.T
getClient :: T Client.T
getAddr :: T Addr.T
getName :: T String
getCapability :: T Port.Cap
getMidiChannels :: T Word
getMidiVoices :: T Word
getSynthVoices :: T Word
getPortSpecified :: T Bool
getTimestamping :: T Bool
getTimestampReal :: T Bool
getTimestampQueue :: T Queue.T
getReadUse :: T Word
getWriteUse :: T Word

getPort           = liftGet PortInfo.getPort
getClient         = liftGet PortInfo.getClient
getAddr           = liftGet PortInfo.getAddr
getName           = liftGet PortInfo.getName
getCapability     = liftGet PortInfo.getCapability
getMidiChannels   = liftGet PortInfo.getMidiChannels
getMidiVoices     = liftGet PortInfo.getMidiVoices
getSynthVoices    = liftGet PortInfo.getSynthVoices
getPortSpecified  = liftGet PortInfo.getPortSpecified
getTimestamping   = liftGet PortInfo.getTimestamping
getTimestampReal  = liftGet PortInfo.getTimestampReal
getTimestampQueue = liftGet PortInfo.getTimestampQueue

getReadUse        = liftGet PortInfo.getReadUse
getWriteUse       = liftGet PortInfo.getWriteUse


setPort :: Port.T -> T ()
setClient :: Client.T -> T ()
setAddr :: Addr.T -> T ()
setName :: String -> T ()
setCapability :: Port.Cap -> T ()
setMidiChannels :: Word -> T ()
setSynthVoices :: Word -> T ()
setMidiVoices :: Word -> T ()
setPortSpecified :: Bool -> T ()
setTimestamping :: Bool -> T ()
setTimestampReal :: Bool -> T ()
setTimestampQueue :: Queue.T -> T ()

setPort           = liftSet PortInfo.setPort
setClient         = liftSet PortInfo.setClient
setAddr           = liftSet PortInfo.setAddr
setName           = liftSet PortInfo.setName
setCapability     = liftSet PortInfo.setCapability
setMidiChannels   = liftSet PortInfo.setMidiChannels
setSynthVoices    = liftSet PortInfo.setSynthVoices
setMidiVoices     = liftSet PortInfo.setMidiVoices
setPortSpecified  = liftSet PortInfo.setPortSpecified
setTimestamping   = liftSet PortInfo.setTimestamping
setTimestampReal  = liftSet PortInfo.setTimestampReal
setTimestampQueue = liftSet PortInfo.setTimestampQueue
