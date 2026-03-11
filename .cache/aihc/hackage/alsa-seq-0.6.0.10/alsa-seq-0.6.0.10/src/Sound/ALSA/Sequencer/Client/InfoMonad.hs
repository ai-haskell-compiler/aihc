{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Client.InfoMonad (
  T,

  run,
  get,
  modify,

  getClient,
  getType,
  getName,
  getBroadcastFilter,
  getErrorBounce,
  getNumPorts,
  getEventLost,

  setClient,
  setName,
  setBroadcastFilter,
  setErrorBounce,
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.ClientInfo as ClientInfo
import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq

import qualified Control.Monad.Trans.Reader as MR
import Control.Applicative (Applicative, )

import Data.Word (Word, )


newtype T a = Cons (MR.ReaderT ClientInfo.T IO a)
   deriving (Functor, Applicative, Monad)


run :: T a -> ClientInfo.T -> IO a
run (Cons m) = MR.runReaderT m

get :: Seq.T mode -> T a -> IO a
get h m = run m =<< ClientInfo.get h

modify :: Seq.T mode -> T a -> IO a
modify h m = do
  i <- ClientInfo.get h
  a <- run m i
  ClientInfo.set h i
  return a


liftGet :: (ClientInfo.T -> IO a) -> T a
liftGet f = Cons $ MR.ReaderT f

liftSet :: (ClientInfo.T -> b -> IO a) -> b -> T a
liftSet f x = Cons $ MR.ReaderT $ flip f x


getClient :: T Client.T
getType :: T Client.Type
getName :: T String
getBroadcastFilter :: T Bool
getErrorBounce :: T Bool
getNumPorts :: T Word
getEventLost :: T Word

getClient          = liftGet ClientInfo.getClient
getType            = liftGet ClientInfo.getType
getName            = liftGet ClientInfo.getName
getBroadcastFilter = liftGet ClientInfo.getBroadcastFilter
getErrorBounce     = liftGet ClientInfo.getErrorBounce
getNumPorts        = liftGet ClientInfo.getNumPorts
getEventLost       = liftGet ClientInfo.getEventLost


setClient :: Client.T -> T ()
setName :: String -> T ()
setBroadcastFilter :: Bool -> T ()
setErrorBounce :: Bool -> T ()

setClient          = liftSet ClientInfo.setClient
setName            = liftSet ClientInfo.setName
setBroadcastFilter = liftSet ClientInfo.setBroadcastFilter
setErrorBounce     = liftSet ClientInfo.setErrorBounce
