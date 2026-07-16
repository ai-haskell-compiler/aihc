{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( catch#,
    compareInt#,
    delay#,
    fork#,
    MVar#,
    MutVar#,
    newMVar#,
    newMutVar#,
    putMVar#,
    raise#,
    realWorld#,
    readMutVar#,
    State#,
    RealWorld,
    takeMVar#,
    ThreadId#,
    TYPE,
    writeMutVar#,
    yield#,
  )
where

import GHC.Types (TYPE)

data State# s

data MutVar# d a

data MVar# d a

data ThreadId#

data RealWorld

foreign import prim raise# :: a -> b

foreign import prim realWorld# :: State# RealWorld

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim newMutVar# :: a -> State# d -> (# State# d, MutVar# d a #)

foreign import prim readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)

foreign import prim writeMutVar# :: MutVar# d a -> a -> State# d -> State# d

foreign import prim
  fork# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, ThreadId# #)

foreign import prim yield# :: State# RealWorld -> State# RealWorld

foreign import prim newMVar# :: State# d -> (# State# d, MVar# d a #)

foreign import prim takeMVar# :: MVar# d a -> State# d -> (# State# d, a #)

foreign import prim putMVar# :: MVar# d a -> a -> State# d -> State# d

foreign import prim delay# :: Int# -> State# d -> State# d

foreign import prim
  catch# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, a #)
