{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
  )
where

import GHC.IO (IO (..))
import GHC.Prim (MVar#, RealWorld, newMVar#, putMVar#, takeMVar#)

data MVar a = MVar (MVar# RealWorld a)

newEmptyMVar :: IO (MVar a)
newEmptyMVar =
  IO
    ( \state ->
        case newMVar# state of
          (# nextState, mvar #) -> (# nextState, MVar mvar #)
    )

newMVar :: a -> IO (MVar a)
newMVar value =
  IO
    ( \state ->
        case newMVar# state of
          (# allocatedState, mvar #) ->
            case putMVar# mvar value allocatedState of
              nextState -> (# nextState, MVar mvar #)
    )

takeMVar :: MVar a -> IO a
takeMVar (MVar mvar) = IO (takeMVar# mvar)

putMVar :: MVar a -> a -> IO ()
putMVar (MVar mvar) value =
  IO
    ( \state ->
        case putMVar# mvar value state of
          nextState -> (# nextState, () #)
    )
