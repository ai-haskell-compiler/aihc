{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( awaitIO#,
    catch#,
    compareInt#,
    fork#,
    MutVar#,
    newMutVar#,
    raise#,
    realWorld#,
    readMutVar#,
    State#,
    ThreadId#,
    RealWorld,
    TYPE,
    writeMutVar#,
    yield#,
  )
where

import GHC.Types (TYPE)

data State# s

data MutVar# d a

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

-- | Suspend the current green thread until an opaque runtime IO request has
-- completed. Concrete IO operations are ordinary runtime foreign calls.
foreign import prim awaitIO# :: Addr# -> State# RealWorld -> State# RealWorld

foreign import prim
  catch# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, a #)
