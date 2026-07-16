{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Concurrent
  ( ThreadId,
    forkIO,
    threadDelay,
    yield,
  )
where

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (ThreadId#, delay#, fork#, yield#)

data ThreadId = ThreadId ThreadId#

forkIO :: IO () -> IO ThreadId
forkIO (IO action) =
  IO
    ( \state ->
        case fork# action state of
          (# nextState, threadId #) -> (# nextState, ThreadId threadId #)
    )

yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )

threadDelay :: Int -> IO ()
threadDelay (I# microseconds) =
  IO
    ( \state ->
        case delay# microseconds state of
          nextState -> (# nextState, () #)
    )
