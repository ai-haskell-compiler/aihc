{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( catch#,
    compareInt#,
    MutVar#,
    newMutVar#,
    raise#,
    realWorld#,
    readMutVar#,
    State#,
    RealWorld,
    TYPE,
    writeMutVar#,
  )
where

import GHC.Types (TYPE)

data State# s

data MutVar# d a

data RealWorld

foreign import prim raise# :: a -> b

foreign import prim realWorld# :: State# RealWorld

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim newMutVar# :: a -> State# d -> (# State# d, MutVar# d a #)

foreign import prim readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)

foreign import prim writeMutVar# :: MutVar# d a -> a -> State# d -> State# d

foreign import prim
  catch# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, a #)
