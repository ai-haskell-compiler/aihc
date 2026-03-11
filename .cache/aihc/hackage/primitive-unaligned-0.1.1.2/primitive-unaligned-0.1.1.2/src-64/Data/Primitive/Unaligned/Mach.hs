{-# language CPP #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Primitive.Unaligned.Mach
  ( indexUnalignedInt64Array#
  , indexUnalignedWord64Array#
  , readUnalignedInt64Array#
  , readUnalignedWord64Array#
  , writeUnalignedInt64Array#
  , writeUnalignedWord64Array#
  ) where

import GHC.Exts (Int#,ByteArray#,MutableByteArray#,State#)
import GHC.Word (Word64(W64#))
import GHC.Int (Int64(I64#))
import qualified GHC.Exts as E

indexUnalignedWord64Array# :: ByteArray# -> Int# -> Word64
indexUnalignedWord64Array# a i =
  W64# (
#if MIN_VERSION_base(4,17,0)
    E.wordToWord64#
#endif
    (E.indexWord8ArrayAsWord# a i))

indexUnalignedInt64Array# :: ByteArray# -> Int# -> Int64
indexUnalignedInt64Array# a i =
  I64# (
#if MIN_VERSION_base(4,17,0)
    E.intToInt64#
#endif
    (E.indexWord8ArrayAsInt# a i))

readUnalignedWord64Array# ::
     MutableByteArray# s
  -> Int#
  -> State# s
  -> (# State# s, Word64 #)
readUnalignedWord64Array# a i s0 =
  case E.readWord8ArrayAsWord# a i s0 of
    (# s1, r #) -> (# s1, W64# (
#if MIN_VERSION_base(4,17,0)
        E.wordToWord64#
#endif
        r)
        #)

readUnalignedInt64Array# ::
     MutableByteArray# s
  -> Int#
  -> State# s
  -> (# State# s, Int64 #)
readUnalignedInt64Array# a i s0 =
  case E.readWord8ArrayAsInt# a i s0 of
    (# s1, r #) -> (# s1, I64# (
#if MIN_VERSION_base(4,17,0)
       E.intToInt64#
#endif
        r) #)

writeUnalignedWord64Array# ::
       MutableByteArray# s
    -> Int#
    -> Word64
    -> State# s
    -> State# s
writeUnalignedWord64Array# a i (W64# w) =
  E.writeWord8ArrayAsWord# a i (
#if MIN_VERSION_base(4,17,0)
    E.word64ToWord#
#endif
    w)

writeUnalignedInt64Array# ::
       MutableByteArray# s
    -> Int#
    -> Int64
    -> State# s
    -> State# s
writeUnalignedInt64Array# a i (I64# w) =
  E.writeWord8ArrayAsInt# a i (
#if MIN_VERSION_base(4,17,0)
    E.int64ToInt#
#endif
    w)
