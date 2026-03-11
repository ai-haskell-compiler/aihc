{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.MemPack.Buffer
-- Copyright   : (c) Alexey Kuleshevich 2024
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack.Buffer where

import Data.Array.Byte
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.ByteString.Internal as BS
import GHC.Exts
import GHC.ST
import GHC.ForeignPtr

-- | Immutable memory buffer
class Buffer b where
  bufferByteCount :: b -> Int

  buffer :: b -> (ByteArray# -> a) -> (Addr# -> a) -> a

instance Buffer ByteArray where
  bufferByteCount (ByteArray ba#) = I# (sizeofByteArray# ba#)
  {-# INLINE bufferByteCount #-}

  buffer (ByteArray ba#) f _ = f ba#
  {-# INLINE buffer #-}

instance Buffer SBS.ShortByteString where
  bufferByteCount = SBS.length
  {-# INLINE bufferByteCount #-}

  buffer (SBS.SBS ba#) f _ = f ba#
  {-# INLINE buffer #-}

instance Buffer BS.ByteString where
  bufferByteCount = BS.length
  {-# INLINE bufferByteCount #-}

  buffer bs _ f =
    runST $ withPtrByteStringST bs $ \(Ptr addr#) -> pure $! f addr#
  {-# INLINE buffer #-}


newMutableByteArray :: Bool -> Int -> ST s (MutableByteArray s)
newMutableByteArray isPinned (I# len#) =
  ST $ \s# -> case (if isPinned then newPinnedByteArray# else newByteArray#) len# s# of
    (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newMutableByteArray #-}

freezeMutableByteArray :: MutableByteArray d -> ST d ByteArray
freezeMutableByteArray (MutableByteArray mba#) =
  ST $ \s# -> case unsafeFreezeByteArray# mba# s# of
    (# s'#, ba# #) -> (# s'#, ByteArray ba# #)

-- | It is ok to use ByteString withing ST, as long as underlying pointer is never mutated
-- or returned from the supplied action.
withPtrByteStringST :: BS.ByteString -> (Ptr a -> ST s b) -> ST s b
#if MIN_VERSION_bytestring(0,11,0)
withPtrByteStringST (BS.BS (ForeignPtr addr# ptrContents) _) f = do
#else
withPtrByteStringST (BS.PS (ForeignPtr addr0# ptrContents) (I# offset#) _) f = do
  let !addr# = addr0# `plusAddr#` offset#
#endif
  !r <- f (Ptr addr#)
  -- It is safe to use `touch#` within ST, so `unsafeCoerce#` is OK
  ST $ \s# -> (# unsafeCoerce# (touch# ptrContents (unsafeCoerce# s#)), () #)
  pure r
{-# INLINE withPtrByteStringST #-}

pinnedByteArrayToByteString :: ByteArray -> BS.ByteString
pinnedByteArrayToByteString (ByteArray ba#) =
  BS.PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}


byteArrayToShortByteString :: ByteArray -> SBS.ShortByteString
byteArrayToShortByteString (ByteArray ba#) = SBS.SBS ba#
{-# INLINE byteArrayToShortByteString #-}

byteArrayFromShortByteString :: SBS.ShortByteString -> ByteArray
byteArrayFromShortByteString (SBS.SBS ba#) = ByteArray ba#
{-# INLINE byteArrayFromShortByteString #-}
