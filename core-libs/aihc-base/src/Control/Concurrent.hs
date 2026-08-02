{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    yield,
  )
where

import GHC.IO (IO (..))
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import GHC.Prim (ThreadId#, fork#, seq, yield#)

-- | An opaque green-thread identifier.
data ThreadId = ThreadId ThreadId#

-- | Schedule an action on a new green thread.
forkIO :: IO () -> IO ThreadId
forkIO (IO action) =
  IO
    ( \state ->
        -- Explicit GRIN apply does not enter operands, and unpacking the IO
        -- newtype alone does not enter its state transformer.
        seq
          action
          ( case fork# action state of
              (# nextState, threadId #) -> (# nextState, ThreadId threadId #)
          )
    )

-- | Cooperatively yield to the next runnable green thread.
yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )
