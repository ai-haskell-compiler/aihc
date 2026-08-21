{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( (+#),
    (-#),
    (*#),
    (<#),
    (==#),
    addIntC#,
    addWordC#,
    and#,
    Addr#,
    Array#,
    awaitIO#,
    ByteArray#,
    byteArrayContents#,
    catch#,
    casMutVar#,
    Char#,
    compareInt#,
    copyByteArray#,
    copyAddrToByteArray#,
    Double#,
    Float#,
    fork#,
    getSizeofMutableByteArray#,
    indexArray#,
    indexWord8OffAddr#,
    indexWord32OffAddr#,
    indexWord64OffAddr#,
    indexWordArray#,
    int2Word#,
    Int#,
    Int8#,
    Int16#,
    Int32#,
    Int64#,
    chr#,
    isByteArrayPinned#,
    isMutableByteArrayPinned#,
    MVar#,
    MutableArray#,
    MutableByteArray#,
    mutableByteArrayContents#,
    MutVar#,
    newAlignedPinnedByteArray#,
    newArray#,
    newByteArray#,
    newMVar#,
    newMutVar#,
    newPinnedByteArray#,
    noDuplicate#,
    not#,
    ord#,
    or#,
    plusWord#,
    popCnt#,
    quotRemWord#,
    quotRemWord2#,
    quotWord#,
    raise#,
    readWordArray#,
    realWorld#,
    readMVar#,
    readArray#,
    readMutVar#,
    resizeMutableByteArray#,
    seq,
    sameMutVar#,
    sameMutableArray#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    subIntC#,
    subWordC#,
    State#,
    StableName#,
    takeMVar#,
    ThreadId#,
    RealWorld,
    TYPE,
    unsafeFreezeByteArray#,
    unsafeFreezeArray#,
    unsafeThawByteArray#,
    unsafeThawArray#,
    makeStableName#,
    stableNameToInt#,
    putMVar#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    unsafeCoerce#,
    word2Int#,
    word8ToWord#,
    word32ToWord#,
    word64ToWord#,
    writeWordArray#,
    writeArray#,
    writeMutVar#,
    Word#,
    Word8#,
    Word16#,
    Word32#,
    Word64#,
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

import GHC.Types (Levity (..), RuntimeRep (..), TYPE, Type, UnliftedType)

data Int#

data Int8#

data Int16#

data Int32#

data Int64#

data Word#

data Word8#

data Word16#

data Word32#

data Word64#

data Char#

data Float#

data Double#

data State# s

data Addr#

data Array# a

data ByteArray#

data MutableArray# d a

data MutableByteArray# d

data MVar# d a

type MutVar# :: Type -> Type -> UnliftedType
data MutVar# d a

data ThreadId#

data StableName# a

data RealWorld

foreign import prim raise# :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b

foreign import prim unsafeCoerce# :: a -> b

foreign import prim seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b

infixr 0 `seq`

foreign import prim realWorld# :: State# RealWorld

foreign import prim noDuplicate# :: State# d -> State# d

foreign import prim makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)

foreign import prim stableNameToInt# :: StableName# a -> Int#

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

foreign import prim (<#) :: Int# -> Int# -> Int#

foreign import prim (==#) :: Int# -> Int# -> Int#

foreign import prim ord# :: Char# -> Int#

foreign import prim chr# :: Int# -> Char#

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

foreign import prim word8ToWord# :: Word8# -> Word#

foreign import prim word32ToWord# :: Word32# -> Word#

foreign import prim word64ToWord# :: Word64# -> Word#

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

foreign import prim newMVar# :: State# d -> (# State# d, MVar# d a #)

foreign import prim readMVar# :: MVar# d a -> State# d -> (# State# d, a #)

foreign import prim takeMVar# :: MVar# d a -> State# d -> (# State# d, a #)

foreign import prim putMVar# :: MVar# d a -> a -> State# d -> State# d

foreign import prim readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)

foreign import prim writeMutVar# :: MutVar# d a -> a -> State# d -> State# d

-- | Replace a mutable variable when its current value is pointer-identical to
-- the expected value. The flag is @0#@ when the swap succeeds and @1#@ when it
-- fails; the final field is the value left in the mutable variable.
foreign import prim casMutVar# :: MutVar# d a -> a -> a -> State# d -> (# State# d, Int#, a #)

foreign import prim sameMutVar# :: MutVar# d a -> MutVar# d a -> Int#

foreign import prim newArray# :: Int# -> a -> State# d -> (# State# d, MutableArray# d a #)

foreign import prim indexArray# :: Array# a -> Int# -> a

foreign import prim readArray# :: MutableArray# d a -> Int# -> State# d -> (# State# d, a #)

foreign import prim writeArray# :: MutableArray# d a -> Int# -> a -> State# d -> State# d

foreign import prim unsafeFreezeArray# :: MutableArray# d a -> State# d -> (# State# d, Array# a #)

foreign import prim unsafeThawArray# :: Array# a -> State# d -> (# State# d, MutableArray# d a #)

foreign import prim sameMutableArray# :: MutableArray# d a -> MutableArray# d a -> Int#

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

foreign import prim indexWord8OffAddr# :: Addr# -> Int# -> Word8#

foreign import prim indexWord32OffAddr# :: Addr# -> Int# -> Word32#

foreign import prim indexWord64OffAddr# :: Addr# -> Int# -> Word64#

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
