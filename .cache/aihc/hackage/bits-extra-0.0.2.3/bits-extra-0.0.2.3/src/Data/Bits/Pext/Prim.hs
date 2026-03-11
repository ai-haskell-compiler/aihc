{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

{-|
Module      : Data.Bits.Pext.Prim
Description : Primop wrappers for the Parallel Extract operation
Copyright   : (c) John Ky, 2018-2019
License     : BSD-3-Clause
Maintainer  : newhoggy@gmail.com
Stability   : stable
-}
module Data.Bits.Pext.Prim
  ( primPext
  , primPext8
  , primPext16
  , primPext32
  , primPext64
  , fastPextEnabled
  ) where

import GHC.Word

#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
import GHC.Prim
#else
import Data.Bits.Pext.Slow
#endif

primPext
  :: Word
  -> Word
  -> Word
primPext src mask = fromIntegral (primPext64 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext #-}

-- | Parallel extract bits for 'Word64'
--
-- Copies selected bits from 'src' to contiguous low-order bits of the return value;
-- higher-order return value bits are cleared.
--
-- >>> primPext64 1 1
-- 1
primPext64
  :: Word64 -- ^ word from which bits will be extracted
  -> Word64 -- ^ bitmap selecting the bits that are to be extracted
  -> Word64 -- ^ word containing the extracted bits
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPext64 (W64# src#) (W64# mask#) = W64# (pext64# src# mask#)
#else
primPext64 = slowPext
#endif
{-# INLINE primPext64 #-}

-- | Parallel extract bits for 'Word32'
--
-- Copies selected bits from 'src' to contiguous low-order bits of the return value;
-- higher-order return value bits are cleared.
--
-- >>> primPext32 1 1
-- 1
primPext32
  :: Word32 -- ^ word from which bits will be extracted
  -> Word32 -- ^ bitmap selecting the bits that are to be extracted
  -> Word32 -- ^ word containing the extracted bits
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPext32 src mask =
  let !(W# src#)   = fromIntegral src  in
  let !(W# mask#)  = fromIntegral mask in
  fromIntegral (W# (pext32# src# mask#))
#else
primPext32 = slowPext
#endif
{-# INLINE primPext32 #-}

-- | Parallel extract bits for 'Word16'
--
-- Copies selected bits from 'src' to contiguous low-order bits of the return value;
-- higher-order return value bits are cleared.
--
-- >>> primPext16 1 1
-- 1
primPext16
  :: Word16 -- ^ word from which bits will be extracted
  -> Word16 -- ^ bitmap selecting the bits that are to be extracted
  -> Word16 -- ^ word containing the extracted bits
primPext16 src mask = fromIntegral (primPext32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext16 #-}

-- | Parallel extract bits for 'Word8'
--
-- Copies selected bits from 'src' to contiguous low-order bits of the return value;
-- higher-order return value bits are cleared.
--
-- >>> primPext8 1 1
-- 1
primPext8
  :: Word8 -- ^ word from which bits will be extracted
  -> Word8 -- ^ bitmap selecting the bits that are to be extracted
  -> Word8 -- ^ word containing the extracted bits
primPext8 src mask = fromIntegral (primPext32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext8 #-}

-- | Runtime flag indicating whether the 'pext' function is using the high-performance
-- BMI2 instruction set.  A value of `False` indicates that `pext` is emulated.
fastPextEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPextEnabled = True
#else
fastPextEnabled = False
#endif
{-# INLINE fastPextEnabled #-}
