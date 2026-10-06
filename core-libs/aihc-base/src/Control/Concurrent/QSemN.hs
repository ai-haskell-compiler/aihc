-- | Quantity semaphores with a quantity for each operation. A thread waits
-- until the requested number of units is available, and gives units back
-- with 'signalQSemN'.
module Control.Concurrent.QSemN
  ( QSemN,
    newQSemN,
    waitQSemN,
    signalQSemN,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception.Base (mask_, onException)
import Control.Monad (join)
import Prelude

-- | A semaphore. It holds the number of free units and, in arrival order,
-- the threads that wait for units. Each waiting thread waits on its own
-- empty variable, together with the number of units that it requests.
newtype QSemN = QSemN (MVar (Int, [(Int, MVar ())]))

-- | Make a semaphore with the given number of free units.
newQSemN :: Int -> IO QSemN
newQSemN initial
  | initial < 0 = fail "newQSemN: Initial quantity must be non-negative"
  | otherwise = QSemN <$> newMVar (initial, [])

-- | Take the given number of units. Wait until that number is free.
waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN state) requested
  | requested < 0 = fail "waitQSemN: requested quantity must be non-negative"
  | otherwise =
      mask_
        $ join
        $ modifyMVar state
        $ \(free, waiters) ->
          if free >= requested
            then return ((free - requested, waiters), return ())
            else do
              waiter <- newEmptyMVar
              return ((free, waiters ++ [(requested, waiter)]), takeMVar waiter `onException` cancel waiter)
  where
    -- An exception stopped the wait. If a signal already gave the units to
    -- this thread, give them back. If not, remove this thread from the
    -- queue, so that no signal gives units to it.
    cancel waiter =
      modifyMVar_ state $ \(free, waiters) -> do
        given <- tryTakeMVar waiter
        case given of
          Just () -> release (free + requested, waiters)
          Nothing -> release (free, filter ((/= waiter) . snd) waiters)

-- | Give back the given number of units. The waiting threads take units in
-- arrival order.
signalQSemN :: QSemN -> Int -> IO ()
signalQSemN (QSemN state) given
  | given < 0 = fail "signalQSemN: signalled quantity must be non-negative"
  | otherwise = mask_ (modifyMVar_ state (\(free, waiters) -> release (free + given, waiters)))

-- | Give free units to the waiting threads in arrival order. A thread that
-- requests more units than are free stays in the queue, and the next
-- threads can take units.
release :: (Int, [(Int, MVar ())]) -> IO (Int, [(Int, MVar ())])
release (free, waiters) =
  case waiters of
    [] -> return (free, [])
    entry@(requested, waiter) : rest
      | requested <= free -> do
          putMVar waiter ()
          release (free - requested, rest)
      | otherwise -> do
          (remaining, kept) <- release (free, rest)
          return (remaining, entry : kept)
