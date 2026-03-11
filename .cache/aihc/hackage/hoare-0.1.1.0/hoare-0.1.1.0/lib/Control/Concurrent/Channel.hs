-- | Tools for connecting communicating sequential processes (threads)
-- with closeable channels.
module Control.Concurrent.Channel(
    Channel (..),
    writeChannel',
    evalWriteChannel,
    evalWriteChannel',
    consumeChannel,
    stateConsumeChannel,
    feedChannel,
    evalFeedChannel,
    pipeline,
    pipeline_
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.TBCQueue
import Control.Exception
import Control.DeepSeq
import Control.Monad

class Channel c where
    -- | Returns @Just value@ until the channel is closed, blocking for the next value.
    readChannel :: c a -> STM (Maybe a)

    -- | Writes the given value to the channel if it's still open.
    --
    -- Returns whether the channel is still open so producers know when consumers no longer care.
    writeChannel :: c a -> a -> STM Bool

    -- | Closes the channel so that future writes are no-ops and readers get @Nothing@
    -- after all values currently inside have been read.
    closeChannel :: c a -> STM ()

    isClosedChannel :: c a -> STM Bool

instance Channel TBCQueue where
    readChannel = readTBCQueue

    writeChannel = writeTBCQueue

    closeChannel = closeTBCQueue

    isClosedChannel = isClosedTBCQueue

-- More to follow? A TMVar-like closeable slot?

-- | Writes to the channel, asserting that it hasn't been closed.
--
-- Useful in situations where only the writer closes the channel (when finished).
writeChannel' :: (Channel c) => c a -> a -> STM ()
writeChannel' c v = do
    o <- writeChannel c v
    unless o $ error "absurd: writeChannel' on closed channel"
{-# INLINE writeChannel' #-}

-- | Force a value to normal form before writing it to the given channel.
--
-- One of the goals of CSP is to divide work into independent tasks.
-- Forcing values before passing them to the next actor can improve performance—it
-- keeps the last one in the chain from doing more than its share of evaluation.
evalWriteChannel :: (Channel c, NFData a) => c a -> a -> IO Bool
evalWriteChannel c v = do
    v' <- evaluate $ force v
    atomically $ writeChannel c v'
{-# INLINE evalWriteChannel #-}

-- | `writeChannel'` meets `evalWriteChannel`
evalWriteChannel' :: (Channel c, NFData a) => c a -> a -> IO ()
evalWriteChannel' c v = do
    v' <- evaluate $ force v
    atomically $ writeChannel' c v'
{-# INLINE evalWriteChannel' #-}

-- | Consume the given channel until it closes,
-- passing values to the given action and collecting its results.
consumeChannel :: (Channel c, Monoid m) => c a -> (a -> IO m) -> IO m
consumeChannel c f = go mempty where
    go !acc = atomically (readChannel c) >>= \case
        Just v -> do
            res <- f v
            go $ acc <> res
        Nothing -> pure acc

-- | Consume the given channel until it closes,
-- with each action updating some state. Returns the final state.
stateConsumeChannel :: (Channel c) => c a -> s -> (s -> a -> IO s) -> IO s
stateConsumeChannel c !state f = atomically (readChannel c) >>= \case
    Just v -> do
        next <- f state v
        stateConsumeChannel c next f
    Nothing -> pure state

feedChannel' :: (Channel c) => c a -> IO (Maybe a) -> IO ()
feedChannel' c f = f >>= \case
    Just v -> do
        stillOpen <- atomically $ writeChannel c v
        when stillOpen $ feedChannel' c f
    Nothing -> atomically $ closeChannel c

-- | Produce values with the given IO action,
-- feeding them into the given channel until it closes or the action returns @Nothing@.
feedChannel :: (Channel c) => c a -> IO (Maybe a) -> IO ()
feedChannel c f = do
    -- Protect against an f that could block for a long time (e.g., a network socket),
    -- even after the consumer hangs up.
    let chanClosed = atomically $ isClosedChannel c >>= check
    race_ (feedChannel' c f) chanClosed

-- | `feedChannel`, but forces produced values.
--
-- See `evalWriteChannel` for the motivation.
evalFeedChannel :: (Channel c, NFData a) => c a -> IO (Maybe a) -> IO ()
evalFeedChannel c f = feedChannel c f' where
    f' = f >>= evaluate . force
{-# INLINE evalFeedChannel #-}

-- | Create a channel with the given action,
-- then wire it to the given producer and consumer.
pipeline :: (Channel c) => IO (c a) -> (c a -> IO x) -> (c a -> IO y) -> IO (x, y)
pipeline new producer consumer = do
    c <- new
    let producer' = producer c `finally` atomically (closeChannel c)
    concurrently producer' (consumer c)

-- | `pipeline`, but ignore the results.
pipeline_ :: (Channel c) => IO (c a) -> (c a -> IO x) -> (c a -> IO y) -> IO ()
pipeline_ n p c = void $ pipeline n p c
{-# INLINE pipeline_ #-}
