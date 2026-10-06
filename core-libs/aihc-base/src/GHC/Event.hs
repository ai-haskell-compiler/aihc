-- | The event manager and the timer manager.
--
-- GHC runs these managers only in its threaded runtime. The aihc runtime
-- has one OS thread, so 'getSystemEventManager' gives 'Nothing', as in the
-- non-threaded GHC runtime. A caller then uses 'GHC.Conc.threadWaitRead'
-- and 'GHC.Conc.threadWaitWrite'.
--
-- The timer manager uses one green thread for each timeout. That thread
-- waits for a delay from 'registerDelay' or for the cancellation.
module GHC.Event
  ( EventManager,
    getSystemEventManager,
    TimerManager,
    TimeoutCallback,
    TimeoutKey,
    getSystemTimerManager,
    registerTimeout,
    unregisterTimeout,
  )
where

import Control.Monad (when)
import GHC.Conc.IO (registerDelay)
import GHC.Conc.Sync (TVar, atomically, forkIO, newTVarIO, readTVar, retry, writeTVar)
import Prelude (Bool (..), IO, Int, Maybe (..), return, (>>=))

-- | An event manager. The aihc runtime has none.
data EventManager

-- | Get the event manager of the system. The aihc runtime has none.
getSystemEventManager :: IO (Maybe EventManager)
getSystemEventManager = return Nothing

-- | The timer manager.
data TimerManager = TimerManager

-- | The action that a timeout runs.
type TimeoutCallback = IO ()

-- | The key that cancels a timeout.
newtype TimeoutKey = TimeoutKey (TVar Bool)

-- | Get the timer manager of the system.
getSystemTimerManager :: IO TimerManager
getSystemTimerManager = return TimerManager

-- | Run the callback after the given number of microseconds.
registerTimeout :: TimerManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout _ microseconds callback = do
  cancelled <- newTVarIO False
  expired <- registerDelay microseconds
  _ <- forkIO (atomically (fireOrCancel cancelled expired) >>= \fire -> when fire callback)
  return (TimeoutKey cancelled)
  where
    fireOrCancel cancelled expired = do
      stopped <- readTVar cancelled
      case stopped of
        True -> return False
        False -> do
          fired <- readTVar expired
          case fired of
            True -> return True
            False -> retry

-- | Cancel a timeout. The call has no effect after the callback starts.
unregisterTimeout :: TimerManager -> TimeoutKey -> IO ()
unregisterTimeout _ (TimeoutKey cancelled) = atomically (writeTVar cancelled True)
