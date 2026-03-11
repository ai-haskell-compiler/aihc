{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim.Compat (
    -- * Indexing Functions
    indexWord8Array#,indexWord16Array#,indexWord32Array#,indexWord64Array#,
    readWord8Array#,readWord16Array#,readWord32Array#,readWord64Array#,
    writeWord8Array#,writeWord16Array#,writeWord32Array#,writeWord64Array#,
    indexWord8OffAddr#,indexWord16OffAddr#,indexWord32OffAddr#,indexWord64OffAddr#,
    readWord8OffAddr#,readWord16OffAddr#,readWord32OffAddr#,readWord64OffAddr#,
    writeWord8OffAddr#,writeWord16OffAddr#,writeWord32OffAddr#,writeWord64OffAddr#) where

#if MIN_VERSION_ghc_prim(0,8,0)
import GHC.Prim (
    Addr#,ByteArray#,Int#,MutableByteArray#,State#,Word#,
    wordToWord8#,wordToWord16#,wordToWord32#,
    word8ToWord#,word16ToWord#,word32ToWord#
#if MIN_VERSION_ghc_prim(0,9,0)
    ,wordToWord64#,word64ToWord#
#endif
    )
import qualified GHC.Prim as Prim
#else
import GHC.Prim (
    indexWord8Array#,indexWord16Array#,indexWord32Array#,indexWord64Array#,
    readWord8Array#,readWord16Array#,readWord32Array#,readWord64Array#,
    writeWord8Array#,writeWord16Array#,writeWord32Array#,writeWord64Array#,
    indexWord8OffAddr#,indexWord16OffAddr#,indexWord32OffAddr#,indexWord64OffAddr#,
    readWord8OffAddr#,readWord16OffAddr#,readWord32OffAddr#,readWord64OffAddr#,
    writeWord8OffAddr#,writeWord16OffAddr#,writeWord32OffAddr#,writeWord64OffAddr#)
#endif


#if MIN_VERSION_ghc_prim(0,8,0)

#if !MIN_VERSION_ghc_prim(0,9,0)
wordToWord64#, word64ToWord# :: Word# -> Word#
wordToWord64# w = w
word64ToWord# w = w
{-# INLINE wordToWord64# #-}
{-# INLINE word64ToWord# #-}
#endif

indexWord8Array# :: ByteArray# -> Int# -> Word#
indexWord8Array# arr i = word8ToWord# (Prim.indexWord8Array# arr i)
{-# INLINE indexWord8Array# #-}

indexWord16Array# :: ByteArray# -> Int# -> Word#
indexWord16Array# arr i = word16ToWord# (Prim.indexWord16Array# arr i)
{-# INLINE indexWord16Array# #-}

indexWord32Array# :: ByteArray# -> Int# -> Word#
indexWord32Array# arr i = word32ToWord# (Prim.indexWord32Array# arr i)
{-# INLINE indexWord32Array# #-}

indexWord64Array# :: ByteArray# -> Int# -> Word#
indexWord64Array# arr i = word64ToWord# (Prim.indexWord64Array# arr i)
{-# INLINE indexWord64Array# #-}

readWord8Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8Array# arr ix st =
  let !(# st', v #) = Prim.readWord8Array# arr ix st
   in (# st', word8ToWord# v #)
{-# INLINE readWord8Array# #-}

readWord16Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord16Array# arr ix st =
  let !(# st', v #) = Prim.readWord16Array# arr ix st
   in (# st', word16ToWord# v #)
{-# INLINE readWord16Array# #-}

readWord32Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord32Array# arr ix st =
  let !(# st', v #) = Prim.readWord32Array# arr ix st
   in (# st', word32ToWord# v #)
{-# INLINE readWord32Array# #-}

readWord64Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord64Array# arr ix st =
  let !(# st', v #) = Prim.readWord64Array# arr ix st
   in (# st', word64ToWord# v #)
{-# INLINE readWord64Array# #-}

writeWord8Array# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8Array# arr ix v st = Prim.writeWord8Array# arr ix (wordToWord8# v) st
{-# INLINE writeWord8Array# #-}

writeWord16Array# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord16Array# arr ix v st = Prim.writeWord16Array# arr ix (wordToWord16# v) st
{-# INLINE writeWord16Array# #-}

writeWord32Array# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord32Array# arr ix v st = Prim.writeWord32Array# arr ix (wordToWord32# v) st
{-# INLINE writeWord32Array# #-}

writeWord64Array# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord64Array# arr ix v st = Prim.writeWord64Array# arr ix (wordToWord64# v) st
{-# INLINE writeWord64Array# #-}

indexWord8OffAddr# :: Addr# -> Int# -> Word#
indexWord8OffAddr# addr off = word8ToWord# (Prim.indexWord8OffAddr# addr off)
{-# INLINE indexWord8OffAddr# #-}

indexWord16OffAddr# :: Addr# -> Int# -> Word#
indexWord16OffAddr# addr off = word16ToWord# (Prim.indexWord16OffAddr# addr off)
{-# INLINE indexWord16OffAddr# #-}

indexWord32OffAddr# :: Addr# -> Int# -> Word#
indexWord32OffAddr# addr off = word32ToWord# (Prim.indexWord32OffAddr# addr off)
{-# INLINE indexWord32OffAddr# #-}

indexWord64OffAddr# :: Addr# -> Int# -> Word#
indexWord64OffAddr# addr off = word64ToWord# (Prim.indexWord64OffAddr# addr off)
{-# INLINE indexWord64OffAddr# #-}

readWord8OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord8OffAddr# addr off st =
  let !(# st', v #) = Prim.readWord8OffAddr# addr off st
   in (# st', word8ToWord# v #)
{-# INLINE readWord8OffAddr# #-}

readWord16OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord16OffAddr# addr off st =
  let !(# st', v #) = Prim.readWord16OffAddr# addr off st
   in (# st', word16ToWord# v #)
{-# INLINE readWord16OffAddr# #-}

readWord32OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord32OffAddr# addr off st =
  let !(# st', v #) = Prim.readWord32OffAddr# addr off st
   in (# st', word32ToWord# v #)
{-# INLINE readWord32OffAddr# #-}

readWord64OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord64OffAddr# addr off st =
  let !(# st', v #) = Prim.readWord64OffAddr# addr off st
   in (# st', word64ToWord# v #)
{-# INLINE readWord64OffAddr# #-}

writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# d -> State# d
writeWord8OffAddr# addr off v st =
  Prim.writeWord8OffAddr# addr off (wordToWord8# v) st
{-# INLINE writeWord8OffAddr# #-}

writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# d -> State# d
writeWord16OffAddr# addr off v st =
  Prim.writeWord16OffAddr# addr off (wordToWord16# v) st
{-# INLINE writeWord16OffAddr# #-}

writeWord32OffAddr# :: Addr# -> Int# -> Word# -> State# d -> State# d
writeWord32OffAddr# addr off v st =
  Prim.writeWord32OffAddr# addr off (wordToWord32# v) st
{-# INLINE writeWord32OffAddr# #-}

writeWord64OffAddr# :: Addr# -> Int# -> Word# -> State# d -> State# d
writeWord64OffAddr# addr off v st =
  Prim.writeWord64OffAddr# addr off (wordToWord64# v) st
{-# INLINE writeWord64OffAddr# #-}

#endif
