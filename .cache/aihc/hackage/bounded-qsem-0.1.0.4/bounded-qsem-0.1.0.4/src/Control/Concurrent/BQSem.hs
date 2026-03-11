
{-# LANGUAGE Safe #-}

-- | Bounded quantity semaphores.
module Control.Concurrent.BQSem
  ( BQSem
  , newBQSem
  , waitBQSem
  , signalBQSem
  , getBQSemQuantity
    ) where

import Control.Concurrent.QSem
import Control.Concurrent.MVar
import Control.Exception (mask, onException)
import Control.Monad (unless)

-- | Bounded quantity semaphore in which the resource is acquired and released in units of one,
--   but with a maximum amount of units available at any given time.
data BQSem = BQSem
  { -- | Underlying unbounded quantity semaphore.
    unboundedQSem :: QSem
    -- | Maximum number of units.
  , bqsemBound :: Int
    -- | Counter of current units.
  , bqsemCounter :: MVar Int
    }

-- | Build a new 'BQSem' with supplied initial and maximum supply.
--   An exception is thrown in any of the following cases:
--
--   * Initial supply is negative.
--   * Maximum supply is less than 1.
--   * Initial supply exceeds maximum.
--
newBQSem
  :: Int -- ^ Initial unit supply.
  -> Int -- ^ Maximum unit supply.
  -> IO BQSem
newBQSem n0 m = do
  unless (n0 <= m) $ fail "newBQSem: Initial quantity must be less or equal than maximum."
  unless (m > 0) $ fail "newBQSem: Maximum quantity must be at least 1."
  qsem <- newQSem n0
  counter <- newMVar n0
  pure $ BQSem
    { unboundedQSem = qsem
    , bqsemBound = m
    , bqsemCounter = counter
      }

-- | Wait for a unit to become available.
waitBQSem :: BQSem -> IO ()
waitBQSem bqsem =
  mask $ \restore -> do
    restore $ waitQSem $ unboundedQSem bqsem
    let counter = bqsemCounter bqsem
    takeMVar counter >>=
      \n -> putMVar counter $! n - 1

-- | Make a new unit available, unless the maximum number of units has been reached,
--   in which case it does nothing (it doesn't block).
signalBQSem :: BQSem -> IO ()
signalBQSem bqsem =
  mask $ \restore -> do
    let counter = bqsemCounter bqsem
    n <- takeMVar counter
    if n == bqsemBound bqsem
       then putMVar counter n
       else do restore (signalQSem $ unboundedQSem bqsem) `onException` putMVar counter n
               putMVar counter $! n + 1

-- | Get current supply quantity.
getBQSemQuantity :: BQSem -> IO Int
getBQSemQuantity = readMVar . bqsemCounter
