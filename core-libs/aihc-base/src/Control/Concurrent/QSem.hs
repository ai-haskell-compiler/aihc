-- | Quantity semaphores. A thread waits until a unit of the resource is
-- available, and gives the unit back with 'signalQSem'.
module Control.Concurrent.QSem
  ( QSem,
    newQSem,
    waitQSem,
    signalQSem,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception.Base (mask_, onException)
import Control.Monad (join)
import Prelude

-- | A semaphore. It holds the number of free units and, in arrival order,
-- the threads that wait for a unit. Each waiting thread waits on its own
-- empty variable.
newtype QSem = QSem (MVar (Int, [MVar ()]))

-- | Make a semaphore with the given number of free units.
newQSem :: Int -> IO QSem
newQSem initial
  | initial < 0 = fail "newQSem: Initial quantity must be non-negative"
  | otherwise = QSem <$> newMVar (initial, [])

-- | Take one unit. Wait when no unit is free.
waitQSem :: QSem -> IO ()
waitQSem (QSem state) =
  mask_
    $ join
    $ modifyMVar state
    $ \(free, waiters) ->
      if free > 0
        then return ((free - 1, waiters), return ())
        else do
          waiter <- newEmptyMVar
          return ((free, waiters ++ [waiter]), takeMVar waiter `onException` cancel waiter)
  where
    -- An exception stopped the wait. If a signal already gave the unit to
    -- this thread, give it to the next waiter. If not, remove this thread
    -- from the queue, so that no signal gives a unit to it.
    cancel waiter =
      modifyMVar_ state $ \(free, waiters) -> do
        given <- tryTakeMVar waiter
        case given of
          Just () -> release (free, waiters)
          Nothing -> return (free, filter (/= waiter) waiters)

-- | Give back one unit. The first waiting thread, if any, takes it.
signalQSem :: QSem -> IO ()
signalQSem (QSem state) = mask_ (modifyMVar_ state release)

release :: (Int, [MVar ()]) -> IO (Int, [MVar ()])
release (free, waiters) =
  case waiters of
    [] -> return (free + 1, [])
    waiter : rest -> do
      putMVar waiter ()
      return (free, rest)
