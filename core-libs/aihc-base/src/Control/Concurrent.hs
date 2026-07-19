{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent
  ( forkIO,
    yield,
  )
where

import GHC.IO (IO (..))
import GHC.Prim (fork#, yield#)

-- | Schedule an action on a new green thread, discarding its result.
forkIO :: IO a -> IO ()
forkIO (IO action) =
  IO
    ( \state ->
        -- Explicit GRIN apply does not enter operands, and unpacking the IO
        -- newtype alone does not enter its state transformer.
        case action of
          forcedAction ->
            case fork# forcedAction state of
              (# nextState, _ #) -> (# nextState, () #)
    )

-- | Cooperatively yield to the next runnable green thread.
yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )
