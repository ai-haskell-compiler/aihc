{-# LANGUAGE
    DataKinds
  , KindSignatures
  #-}

module Control.Concurrent.Chan.Typed where

import Control.Concurrent.Chan.Scope (Scope (..), Writable, Readable)

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan


newtype ChanRW (scope :: Scope) a = ChanRW (Chan a)


newChanRW :: IO (ChanRW 'ReadWrite a)
newChanRW = ChanRW <$> Chan.newChan


writeChanRW :: Writable scope => ChanRW scope a -> a -> IO ()
writeChanRW (ChanRW c) x = Chan.writeChan c x


readChanRW :: Readable scope => ChanRW scope a -> IO a
readChanRW (ChanRW c) = Chan.readChan c


dupChanRW :: Writable scopeIn
          => Readable scopeOut
          => ChanRW scopeIn a -> IO (ChanRW scopeOut a)
dupChanRW (ChanRW c) = ChanRW <$> Chan.dupChan c
