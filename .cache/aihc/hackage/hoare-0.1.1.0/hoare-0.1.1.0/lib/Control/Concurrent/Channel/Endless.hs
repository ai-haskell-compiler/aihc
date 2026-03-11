-- | Tools for connecting communicating sequential processes (threads)
-- with endless channels.
--
-- Many useful programs have no end to their input—they run in a steady state until killed.
-- This module provides the same utilities as "Control.Concurrent.Channel",
-- but for standard STM types we know and love.
module Control.Concurrent.Channel.Endless(
    Channel (..),
    evalWriteChannel,
    consumeChannel,
    feedChannel,
    evalFeedChannel,
    pipeline,
    pipeline_
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq
import Control.Monad

class Channel c where
    readChannel :: c a -> STM a

    writeChannel :: c a -> a -> STM ()

instance Channel TBQueue where
    readChannel = readTBQueue

    writeChannel = writeTBQueue

instance Channel TMVar where
    readChannel = takeTMVar

    writeChannel = writeTMVar

evalWriteChannel :: (Channel c, NFData a) => c a -> a -> IO ()
evalWriteChannel c v = do
    v' <- evaluate $ force v
    atomically $ writeChannel c v'
{-# INLINE evalWriteChannel #-}

consumeChannel :: (Channel c) => c a -> (a -> IO ()) -> IO ()
consumeChannel c f = forever $ atomically (readChannel c) >>= f

feedChannel :: (Channel c) => c a -> IO a -> IO ()
feedChannel c f = forever $ f >>= atomically . writeChannel c

-- | `feedChannel`, but forces produced values.
--
-- See `evalWriteChannel` for the motivation.
evalFeedChannel :: (Channel c, NFData a) => c a -> IO a -> IO ()
evalFeedChannel c f = feedChannel c f' where
    f' = f >>= evaluate . force
{-# INLINE evalFeedChannel #-}

-- There's not much to these when you don't ever close, but for completeness:

-- | Create a channel with the given action,
-- then wire it to the given producer and consumer.
pipeline :: IO c -> (c -> IO x) -> (c -> IO y) -> IO (x, y)
pipeline new producer consumer = do
    c <- new
    concurrently (producer c) (consumer c)

-- | `pipeline`, but ignore the results.
pipeline_ :: IO c -> (c -> IO x) -> (c -> IO y) -> IO ()
pipeline_ n p c = void $ pipeline n p c
{-# INLINE pipeline_ #-}
