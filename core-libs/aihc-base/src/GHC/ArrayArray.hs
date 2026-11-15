{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | Arrays of unlifted arrays, as a wrapper around 'Array#' of 'ByteArray#'.
-- A 'ByteArray#' element can also hold an 'ArrayArray#' or a
-- 'MutableArrayArray#'. The operations on those elements cast the element.
module GHC.ArrayArray
  ( ArrayArray# (..),
    MutableArrayArray# (..),
    newArrayArray#,
    unsafeFreezeArrayArray#,
    sizeofArrayArray#,
    sizeofMutableArrayArray#,
    indexByteArrayArray#,
    indexArrayArrayArray#,
    readByteArrayArray#,
    readMutableByteArrayArray#,
    readArrayArrayArray#,
    readMutableArrayArrayArray#,
    writeByteArrayArray#,
    writeMutableByteArrayArray#,
    writeArrayArrayArray#,
    writeMutableArrayArrayArray#,
    copyArrayArray#,
    copyMutableArrayArray#,
    sameArrayArray#,
    sameMutableArrayArray#,
  )
where

import GHC.Prim
import GHC.Types (Type, UnliftedType)

type ArrayArray# :: UnliftedType
newtype ArrayArray# = ArrayArray# (Array# ByteArray#)

type role MutableArrayArray# nominal

type MutableArrayArray# :: Type -> UnliftedType
newtype MutableArrayArray# s = MutableArrayArray# (MutableArray# s ByteArray#)

-- | Allocate an array. Each element is an empty 'ByteArray#' until a write
-- replaces it.
newArrayArray# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
newArrayArray# size s0 =
  case newByteArray# 0# s0 of
    (# s1, emptyBytes #) -> case unsafeFreezeByteArray# emptyBytes s1 of
      (# s2, empty #) -> case newArray# size empty s2 of
        (# s3, array #) -> (# s3, MutableArrayArray# array #)

unsafeFreezeArrayArray# :: MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
unsafeFreezeArrayArray# (MutableArrayArray# array) s0 =
  case unsafeFreezeArray# array s0 of
    (# s1, frozen #) -> (# s1, ArrayArray# frozen #)

sizeofArrayArray# :: ArrayArray# -> Int#
sizeofArrayArray# (ArrayArray# array) = sizeofArray# array

sizeofMutableArrayArray# :: MutableArrayArray# s -> Int#
sizeofMutableArrayArray# (MutableArrayArray# array) = sizeofMutableArray# array

indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
indexByteArrayArray# (ArrayArray# array) index =
  case indexArray# array index of
    (# element #) -> element

indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
indexArrayArrayArray# (ArrayArray# array) index =
  case indexArray# array index of
    (# element #) -> unsafeCoerce# element

readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
readByteArrayArray# (MutableArrayArray# array) index s0 = readArray# array index s0

readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
readMutableByteArrayArray# (MutableArrayArray# array) index s0 =
  case readArray# array index s0 of
    (# s1, element #) -> (# s1, unsafeCoerce# element #)

readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
readArrayArrayArray# (MutableArrayArray# array) index s0 =
  case readArray# array index s0 of
    (# s1, element #) -> (# s1, unsafeCoerce# element #)

readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
readMutableArrayArrayArray# (MutableArrayArray# array) index s0 =
  case readArray# array index s0 of
    (# s1, element #) -> (# s1, unsafeCoerce# element #)

writeByteArrayArray# :: MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
writeByteArrayArray# (MutableArrayArray# array) index element s0 = writeArray# array index element s0

writeMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
writeMutableByteArrayArray# (MutableArrayArray# array) index element s0 =
  writeArray# array index (unsafeCoerce# element) s0

writeArrayArrayArray# :: MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
writeArrayArrayArray# (MutableArrayArray# array) index element s0 =
  writeArray# array index (unsafeCoerce# element) s0

writeMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
writeMutableArrayArrayArray# (MutableArrayArray# array) index element s0 =
  writeArray# array index (unsafeCoerce# element) s0

copyArrayArray# :: ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyArrayArray# (ArrayArray# source) sourceOffset (MutableArrayArray# target) targetOffset count s0 =
  copyArray# source sourceOffset target targetOffset count s0

copyMutableArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableArrayArray# (MutableArrayArray# source) sourceOffset (MutableArrayArray# target) targetOffset count s0 =
  copyMutableArray# source sourceOffset target targetOffset count s0

sameArrayArray# :: ArrayArray# -> ArrayArray# -> Int#
sameArrayArray# (ArrayArray# left) (ArrayArray# right) = reallyUnsafePtrEquality# left right

sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Int#
sameMutableArrayArray# (MutableArrayArray# left) (MutableArrayArray# right) = sameMutableArray# left right
