-- | Missing @byteSwap@ functions for signed integers.
--
-- We have them for unsigned integers, but not for signed.
-- They should probably be provided, so I'm considering this a compatibility
-- module for the future when we have them.

module Raehik.Compat.Data.Int.ByteSwap where

import GHC.Exts
import GHC.Int

byteSwapI16 :: Int16 -> Int16
byteSwapI16 (I16# i#) = I16# (word16ToInt16# (wordToWord16# (byteSwap16# (word16ToWord# (int16ToWord16# i#)))))
{-# INLINE byteSwapI16 #-}

byteSwapI32 :: Int32 -> Int32
byteSwapI32 (I32# i#) = I32# (word32ToInt32# (wordToWord32# (byteSwap32# (word32ToWord# (int32ToWord32# i#)))))
{-# INLINE byteSwapI32 #-}

byteSwapI64 :: Int64 -> Int64
byteSwapI64 (I64# i#) = I64# (word64ToInt64# (byteSwap64# (int64ToWord64# i#)))
{-# INLINE byteSwapI64 #-}

byteSwapI :: Int -> Int
byteSwapI (I# i#) = I# (word2Int# (byteSwap# (int2Word# i#)))
{-# INLINE byteSwapI #-}
