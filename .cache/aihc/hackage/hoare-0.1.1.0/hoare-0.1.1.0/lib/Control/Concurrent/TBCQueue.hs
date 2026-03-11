-- | A /closeable/, bounded STM queue.
module Control.Concurrent.TBCQueue(
    TBCQueue,
    newTBCQueue,
    newTBCQueueIO,
    readTBCQueue,
    writeTBCQueue,
    closeTBCQueue,
    isOpenTBCQueue,
    isClosedTBCQueue,
    lengthTBCQueue
) where

import Control.Concurrent.STM
import Control.Monad
import Numeric.Natural

data TBCQueue a = TBCQueue {
    q :: !(TBQueue a),
    open :: !(TVar Bool)
}

newTBCQueue :: Natural -> STM (TBCQueue a)
newTBCQueue n = TBCQueue <$> newTBQueue n <*> newTVar True

newTBCQueueIO :: Natural -> IO (TBCQueue a)
newTBCQueueIO = atomically . newTBCQueue

-- | Returns @Just value@ until the queue is closed, blocking for the next value.
readTBCQueue :: TBCQueue a -> STM (Maybe a)
readTBCQueue c = do
    mv <- tryReadTBQueue c.q
    case mv of
        Just v -> pure $ Just v
        Nothing -> do
            stillOpen <- readTVar c.open
            if stillOpen
                then retry
                else pure Nothing

-- | Writes the given value to the queue if it's still open.
--
-- Returns whether the queue is still open so producers know when consumers no longer care.
writeTBCQueue :: TBCQueue a -> a -> STM Bool
writeTBCQueue c !v = do
    stillOpen <- readTVar c.open
    when stillOpen $ writeTBQueue c.q v
    pure stillOpen

-- | Closes the queue so that future writes are no-ops and readers get @Nothing@
-- after all values currently inside have been read.
closeTBCQueue :: TBCQueue a -> STM ()
closeTBCQueue c = writeTVar c.open False

isOpenTBCQueue :: TBCQueue a -> STM Bool
isOpenTBCQueue c = readTVar c.open

isClosedTBCQueue :: TBCQueue a -> STM Bool
isClosedTBCQueue = fmap not . isOpenTBCQueue

-- | The number of items currently in the queue.
--
-- Be careful using this outside the current STM transaction as it is obviously racy.
lengthTBCQueue :: TBCQueue a -> STM Natural
lengthTBCQueue c = lengthTBQueue c.q
