{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
where

import GHC.IO (IO (..))
import GHC.Prim (MVar#, RealWorld, newMVar#, putMVar#, readMVar#, takeMVar#)

-- | A synchronized mutable location that is either empty or contains one
-- value.
data MVar a = MVar (MVar# RealWorld a)

-- | Create an empty 'MVar'.
newEmptyMVar :: IO (MVar a)
newEmptyMVar =
  IO
    ( \state ->
        case newMVar# state of
          (# nextState, mvar #) -> (# nextState, MVar mvar #)
    )

-- | Create a full 'MVar' containing the supplied value.
newMVar :: a -> IO (MVar a)
newMVar value =
  IO
    ( \state ->
        case newMVar# state of
          (# nextState, mvar #) ->
            case putMVar# mvar value nextState of
              finalState -> (# finalState, MVar mvar #)
    )

-- | Read the current value without emptying the 'MVar', blocking when empty.
readMVar :: MVar a -> IO a
readMVar (MVar mvar) =
  IO
    ( \state ->
        case readMVar# mvar state of
          (# nextState, value #) -> (# nextState, value #)
    )

-- | Remove and return the current value, blocking when empty.
takeMVar :: MVar a -> IO a
takeMVar (MVar mvar) =
  IO
    ( \state ->
        case takeMVar# mvar state of
          (# nextState, value #) -> (# nextState, value #)
    )

-- | Fill an empty 'MVar', blocking while it remains full.
putMVar :: MVar a -> a -> IO ()
putMVar (MVar mvar) value =
  IO
    ( \state ->
        case putMVar# mvar value state of
          nextState -> (# nextState, () #)
    )
