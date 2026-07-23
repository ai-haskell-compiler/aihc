{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( addIntC#,
    addWordC#,
    and#,
    awaitIO#,
    ByteArray#,
    byteArrayContents#,
    catch#,
    compareInt#,
    copyByteArray#,
    copyAddrToByteArray#,
    fork#,
    getSizeofMutableByteArray#,
    indexWordArray#,
    int2Word#,
    isByteArrayPinned#,
    isMutableByteArrayPinned#,
    MutableByteArray#,
    mutableByteArrayContents#,
    MutVar#,
    newAlignedPinnedByteArray#,
    newByteArray#,
    newMutVar#,
    newPinnedByteArray#,
    not#,
    or#,
    plusWord#,
    popCnt#,
    quotRemWord#,
    quotRemWord2#,
    quotWord#,
    raise#,
    readWordArray#,
    realWorld#,
    readMutVar#,
    resizeMutableByteArray#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    subIntC#,
    subWordC#,
    State#,
    ThreadId#,
    RealWorld,
    TYPE,
    unsafeFreezeByteArray#,
    unsafeThawByteArray#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    word2Int#,
    writeWordArray#,
    writeMutVar#,
    xor#,
    clz#,
    ctz#,
    eqWord#,
    geWord#,
    gtWord#,
    leWord#,
    ltWord#,
    minusWord#,
    neWord#,
    remWord#,
    timesWord#,
    timesWord2#,
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

foreign import prim addIntC# :: Int# -> Int# -> (# Int#, Int# #)

foreign import prim subIntC# :: Int# -> Int# -> (# Int#, Int# #)

foreign import prim plusWord# :: Word# -> Word# -> Word#

foreign import prim minusWord# :: Word# -> Word# -> Word#

foreign import prim timesWord# :: Word# -> Word# -> Word#

foreign import prim addWordC# :: Word# -> Word# -> (# Word#, Int# #)

foreign import prim subWordC# :: Word# -> Word# -> (# Word#, Int# #)

foreign import prim timesWord2# :: Word# -> Word# -> (# Word#, Word# #)

foreign import prim quotWord# :: Word# -> Word# -> Word#

foreign import prim remWord# :: Word# -> Word# -> Word#

foreign import prim quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)

foreign import prim quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)

foreign import prim and# :: Word# -> Word# -> Word#

foreign import prim or# :: Word# -> Word# -> Word#

foreign import prim xor# :: Word# -> Word# -> Word#

foreign import prim not# :: Word# -> Word#

foreign import prim uncheckedShiftL# :: Word# -> Int# -> Word#

foreign import prim uncheckedShiftRL# :: Word# -> Int# -> Word#

foreign import prim int2Word# :: Int# -> Word#

foreign import prim word2Int# :: Word# -> Int#

foreign import prim eqWord# :: Word# -> Word# -> Int#

foreign import prim neWord# :: Word# -> Word# -> Int#

foreign import prim ltWord# :: Word# -> Word# -> Int#

foreign import prim leWord# :: Word# -> Word# -> Int#

foreign import prim gtWord# :: Word# -> Word# -> Int#

foreign import prim geWord# :: Word# -> Word# -> Int#

foreign import prim clz# :: Word# -> Word#

foreign import prim ctz# :: Word# -> Word#

foreign import prim popCnt# :: Word# -> Word#

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

foreign import prim indexWordArray# :: ByteArray# -> Int# -> Word#

foreign import prim readWordArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)

foreign import prim writeWordArray# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d

foreign import prim copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

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
