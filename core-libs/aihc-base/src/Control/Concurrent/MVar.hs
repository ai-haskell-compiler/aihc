module Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    isEmptyMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
where

import GHC.MVar
  ( MVar,
    isEmptyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
