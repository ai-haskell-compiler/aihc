{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent
  ( ThreadId,
    forkIO,
    yield,
  )
where

import GHC.IO (IO (..))
import GHC.Prim (ThreadId#, fork#, yield#)

-- | An opaque green-thread identifier.
data ThreadId = ThreadId ThreadId#

-- | Schedule an action on a new green thread.
forkIO :: IO () -> IO ThreadId
forkIO (IO action) =
  IO
    ( \state ->
        -- Explicit GRIN apply does not enter operands, and unpacking the IO
        -- newtype alone does not enter its state transformer.
        case action of
          forcedAction ->
            case fork# forcedAction state of
              (# nextState, threadId #) -> (# nextState, ThreadId threadId #)
    )

-- | Cooperatively yield to the next runnable green thread.
yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )
