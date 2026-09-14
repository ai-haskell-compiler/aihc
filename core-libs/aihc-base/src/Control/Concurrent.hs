module Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    yield,
  )
where

import GHC.Conc.Sync (ThreadId, forkIO, yield)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
