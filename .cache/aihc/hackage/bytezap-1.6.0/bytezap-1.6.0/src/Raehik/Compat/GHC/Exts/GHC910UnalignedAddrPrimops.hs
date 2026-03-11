{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

{-
(index|read|write)<ty>OffAddr# primops fail when unaligned on platforms not
supporting unaligned accesses. GHC 9.10 introduces new primops that handle
cases where platforms need aligned accesses. This module imitates that for
older GHCs, but without the safety. So we still fail when unaligned, but it's
easier to upgrade when GHC 9.10 is out.

Note that GC-managed addresses already have these primops. This is for 'Addr#',
non-GC-managed.

Import this module unqualified along with 'GHC.Exts' .

On supported GHCs, we simply re-export. This means unused import warnings.
Sorry idk how to fix that.
-}

module Raehik.Compat.GHC.Exts.GHC910UnalignedAddrPrimops
  ( indexWord8OffAddrAsWord16#
  , indexWord8OffAddrAsWord32#
  , indexWord8OffAddrAsWord64#
  , indexWord8OffAddrAsWord#
  , indexWord8OffAddrAsInt16#
  , indexWord8OffAddrAsInt32#
  , indexWord8OffAddrAsInt64#
  , indexWord8OffAddrAsInt#
  , readWord8OffAddrAsWord16#
  , readWord8OffAddrAsWord32#
  , readWord8OffAddrAsWord64#
  , readWord8OffAddrAsWord#
  , readWord8OffAddrAsInt16#
  , readWord8OffAddrAsInt32#
  , readWord8OffAddrAsInt64#
  , readWord8OffAddrAsInt#
  , writeWord8OffAddrAsWord16#
  , writeWord8OffAddrAsWord32#
  , writeWord8OffAddrAsWord64#
  , writeWord8OffAddrAsWord#
  , writeWord8OffAddrAsInt16#
  , writeWord8OffAddrAsInt32#
  , writeWord8OffAddrAsInt64#
  , writeWord8OffAddrAsInt#
  ) where

import GHC.Exts

#if MIN_VERSION_base(4,20,0)

-- These should all be imported from GHC.Exts, so above is simply re-exporting.

#else

indexWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16#
indexWord8OffAddrAsWord16# addr# os# =
    indexWord16OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsWord16#
    :: Addr# -> Int# -> State# d -> (# State# d, Word16# #)
readWord8OffAddrAsWord16# addr# os# = \s0 ->
    readWord16OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16# -> State# d -> State# d
writeWord8OffAddrAsWord16# addr# os# i# s# =
    writeWord16OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32#
indexWord8OffAddrAsWord32# addr# os# =
    indexWord32OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsWord32#
    :: Addr# -> Int# -> State# d -> (# State# d, Word32# #)
readWord8OffAddrAsWord32# addr# os# = \s0 ->
    readWord32OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32# -> State# d -> State# d
writeWord8OffAddrAsWord32# addr# os# i# s# =
    writeWord32OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64#
indexWord8OffAddrAsWord64# addr# os# =
    indexWord64OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsWord64#
    :: Addr# -> Int# -> State# d -> (# State# d, Word64# #)
readWord8OffAddrAsWord64# addr# os# = \s0 ->
    readWord64OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64# -> State# d -> State# d
writeWord8OffAddrAsWord64# addr# os# i# s# =
    writeWord64OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsWord# :: Addr# -> Int# -> Word#
indexWord8OffAddrAsWord# addr# os# =
    indexWordOffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsWord#
    :: Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord8OffAddrAsWord# addr# os# = \s0 ->
    readWordOffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsWord# :: Addr# -> Int# -> Word# -> State# d -> State# d
writeWord8OffAddrAsWord# addr# os# i# s# =
    writeWordOffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsInt16# :: Addr# -> Int# -> Int16#
indexWord8OffAddrAsInt16# addr# os# =
    indexInt16OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsInt16#
    :: Addr# -> Int# -> State# d -> (# State# d, Int16# #)
readWord8OffAddrAsInt16# addr# os# = \s0 ->
    readInt16OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsInt16# :: Addr# -> Int# -> Int16# -> State# d -> State# d
writeWord8OffAddrAsInt16# addr# os# i# s# =
    writeInt16OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsInt32# :: Addr# -> Int# -> Int32#
indexWord8OffAddrAsInt32# addr# os# =
    indexInt32OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsInt32#
    :: Addr# -> Int# -> State# d -> (# State# d, Int32# #)
readWord8OffAddrAsInt32# addr# os# = \s0 ->
    readInt32OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsInt32# :: Addr# -> Int# -> Int32# -> State# d -> State# d
writeWord8OffAddrAsInt32# addr# os# i# s# =
    writeInt32OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsInt64# :: Addr# -> Int# -> Int64#
indexWord8OffAddrAsInt64# addr# os# =
    indexInt64OffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsInt64#
    :: Addr# -> Int# -> State# d -> (# State# d, Int64# #)
readWord8OffAddrAsInt64# addr# os# = \s0 ->
    readInt64OffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsInt64# :: Addr# -> Int# -> Int64# -> State# d -> State# d
writeWord8OffAddrAsInt64# addr# os# i# s# =
    writeInt64OffAddr# (addr# `plusAddr#` os#) 0# i# s#

indexWord8OffAddrAsInt# :: Addr# -> Int# -> Int#
indexWord8OffAddrAsInt# addr# os# =
    indexIntOffAddr# (addr# `plusAddr#` os#) 0#

readWord8OffAddrAsInt#
    :: Addr# -> Int# -> State# d -> (# State# d, Int# #)
readWord8OffAddrAsInt# addr# os# = \s0 ->
    readIntOffAddr# (addr# `plusAddr#` os#) 0# s0

writeWord8OffAddrAsInt# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeWord8OffAddrAsInt# addr# os# i# s# =
    writeIntOffAddr# (addr# `plusAddr#` os#) 0# i# s#

#endif
