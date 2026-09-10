{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    isEmptyMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
where

import GHC.IO (IO (..))
import GHC.Prim (Int#, MVar#, RealWorld, isEmptyMVar#, newMVar#, putMVar#, readMVar#, takeMVar#, tryPutMVar#, tryReadMVar#, tryTakeMVar#)
import GHC.Prim.Base (Maybe (..))
import GHC.Types (Bool (..), isTrue#)

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

-- | Whether the 'MVar' is currently empty. The answer is stale as soon as
-- another thread runs, so it is only a hint.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar mvar) =
  IO
    ( \state ->
        case isEmptyMVar# mvar state of
          (# nextState, flag #) -> (# nextState, isTrue# flag #)
    )

-- | Take the contents when the 'MVar' is full, without ever blocking.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar mvar) =
  IO
    ( \state ->
        case tryTakeMVar# mvar state of
          (# nextState, flag, value #) -> (# nextState, maybeTaken flag value #)
    )

-- | Read the contents without emptying the 'MVar', without ever blocking.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar mvar) =
  IO
    ( \state ->
        case tryReadMVar# mvar state of
          (# nextState, flag, value #) -> (# nextState, maybeTaken flag value #)
    )

-- | Fill the 'MVar' when it is empty, without ever blocking. The result says
-- whether the value was stored.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar mvar) value =
  IO
    ( \state ->
        case tryPutMVar# mvar value state of
          (# nextState, flag #) -> (# nextState, isTrue# flag #)
    )

-- | The value field of a failed try is undefined, so it is only read when the
-- flag says the 'MVar' was full.
maybeTaken :: Int# -> a -> Maybe a
maybeTaken flag value =
  case isTrue# flag of
    True -> Just value
    False -> Nothing
