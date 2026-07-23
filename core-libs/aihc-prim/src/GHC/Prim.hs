{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( awaitIO#,
    ByteArray#,
    byteArrayContents#,
    catch#,
    compareInt#,
    copyAddrToByteArray#,
    fork#,
    getSizeofMutableByteArray#,
    isByteArrayPinned#,
    isMutableByteArrayPinned#,
    MutableByteArray#,
    mutableByteArrayContents#,
    MutVar#,
    newAlignedPinnedByteArray#,
    newByteArray#,
    newMutVar#,
    newPinnedByteArray#,
    raise#,
    realWorld#,
    readMutVar#,
    resizeMutableByteArray#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    State#,
    ThreadId#,
    RealWorld,
    TYPE,
    unsafeFreezeByteArray#,
    unsafeThawByteArray#,
    writeMutVar#,
    yield#,
  )
where

import GHC.Types (TYPE)

data State# s

data ByteArray#

data MutableByteArray# d

data MutVar# d a

data ThreadId#

data RealWorld

foreign import prim raise# :: a -> b

foreign import prim realWorld# :: State# RealWorld

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim newMutVar# :: a -> State# d -> (# State# d, MutVar# d a #)

foreign import prim readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)

foreign import prim writeMutVar# :: MutVar# d a -> a -> State# d -> State# d

foreign import prim newByteArray# :: Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim newPinnedByteArray# :: Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim newAlignedPinnedByteArray# :: Int# -> Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim isMutableByteArrayPinned# :: MutableByteArray# d -> Int#

foreign import prim isByteArrayPinned# :: ByteArray# -> Int#

foreign import prim byteArrayContents# :: ByteArray# -> Addr#

foreign import prim mutableByteArrayContents# :: MutableByteArray# d -> Addr#

foreign import prim shrinkMutableByteArray# :: MutableByteArray# d -> Int# -> State# d -> State# d

foreign import prim resizeMutableByteArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim unsafeFreezeByteArray# :: MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)

foreign import prim unsafeThawByteArray# :: ByteArray# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim sizeofByteArray# :: ByteArray# -> Int#

foreign import prim getSizeofMutableByteArray# :: MutableByteArray# d -> State# d -> (# State# d, Int# #)

foreign import prim copyAddrToByteArray# :: Addr# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

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
