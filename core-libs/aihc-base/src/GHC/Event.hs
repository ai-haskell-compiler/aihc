-- | The event manager and the timer manager.
--
-- GHC runs these managers only in its threaded runtime. The aihc runtime
-- has one OS thread, so 'getSystemEventManager' gives 'Nothing', as in the
-- non-threaded GHC runtime. A caller then uses 'GHC.Conc.threadWaitRead'
-- and 'GHC.Conc.threadWaitWrite'.
--
-- The timer manager uses one green thread for each timeout. That thread
-- waits for a delay from 'registerDelay' or for the cancellation.
-- 'updateTimeout' puts a new delay in the key. The thread then waits for
-- the new delay.
module GHC.Event
  ( EventManager,
    getSystemEventManager,
    TimerManager,
    TimeoutCallback,
    TimeoutKey,
    getSystemTimerManager,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,
  )
where

import Control.Monad (when)
import Data.Unique (Unique, newUnique)
import GHC.Conc.IO (registerDelay)
import GHC.Conc.Sync (TVar, atomically, forkIO, newTVarIO, readTVar, retry, writeTVar)
import Prelude (Bool (..), Eq (..), IO, Int, Maybe (..), Ord (..), return, (>>=))

-- | An event manager. The aihc runtime has none.
data EventManager

-- | Get the event manager of the system. The aihc runtime has none.
getSystemEventManager :: IO (Maybe EventManager)
getSystemEventManager = return Nothing

-- | The timer manager.
data TimerManager = TimerManager

-- | The action that a timeout runs.
type TimeoutCallback = IO ()

-- | The key that changes or cancels a timeout. The first field identifies
-- the key. The second field is the cancellation flag. The third field holds
-- the flag of the current delay.
data TimeoutKey = TimeoutKey Unique (TVar Bool) (TVar (TVar Bool))

instance Eq TimeoutKey where
  TimeoutKey a _ _ == TimeoutKey b _ _ = a == b

instance Ord TimeoutKey where
  compare (TimeoutKey a _ _) (TimeoutKey b _ _) = compare a b

-- | Get the timer manager of the system.
getSystemTimerManager :: IO TimerManager
getSystemTimerManager = return TimerManager

-- | Run the callback after the given number of microseconds.
registerTimeout :: TimerManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout _ microseconds callback = do
  unique <- newUnique
  cancelled <- newTVarIO False
  expired <- registerDelay microseconds
  current <- newTVarIO expired
  _ <- forkIO (atomically (fireOrCancel cancelled current) >>= \fire -> when fire callback)
  return (TimeoutKey unique cancelled current)
  where
    fireOrCancel cancelled current = do
      stopped <- readTVar cancelled
      case stopped of
        True -> return False
        False -> do
          expired <- readTVar current
          fired <- readTVar expired
          case fired of
            True -> return True
            False -> retry

-- | Set the delay of a timeout to the given number of microseconds from
-- now. The call has no effect after the callback starts.
updateTimeout :: TimerManager -> TimeoutKey -> Int -> IO ()
updateTimeout _ (TimeoutKey _ _ current) microseconds = do
  expired <- registerDelay microseconds
  atomically (writeTVar current expired)

-- | Cancel a timeout. The call has no effect after the callback starts.
unregisterTimeout :: TimerManager -> TimeoutKey -> IO ()
unregisterTimeout _ (TimeoutKey _ cancelled _) = atomically (writeTVar cancelled True)
