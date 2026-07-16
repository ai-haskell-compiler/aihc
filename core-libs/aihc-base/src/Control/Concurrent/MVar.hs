module Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
  )
where

import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
