{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

{-|
Module      : Data.Bits.Pdep.Prim
Description : Primop wrappers for the Parallel Deposit operation
Copyright   : (c) John Ky, 2018-2019
License     : BSD-3-Clause
Maintainer  : newhoggy@gmail.com
Stability   : stable
-}
module Data.Bits.Pdep.Prim
  ( primPdep
  , primPdep8
  , primPdep16
  , primPdep32
  , primPdep64
  , fastPdepEnabled
  ) where

import GHC.Prim
import GHC.Word

#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
#else
import Data.Bits.Pdep.Slow
#endif

-- | Bitwise parallel deposit for 'Word'.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> primPdep 1 1
-- 1
primPdep
  :: Word -- ^ word containing the bits that will be deposited
  -> Word -- ^ bitmap selecting the bits that are to be deposited
  -> Word -- ^ word containing the deposited bits
primPdep src mask = fromIntegral (primPdep64 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep #-}

-- | Bitwise parallel deposit for 'Word64'.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> primPdep64 1 1
-- 1
primPdep64
  :: Word64 -- ^ word containing the bits that will be deposited
  -> Word64 -- ^ bitmap selecting the bits that are to be deposited
  -> Word64 -- ^ word containing the deposited bits
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPdep64 (W64# src#) (W64# mask#) = W64# (pdep64# src# mask#)
#else
primPdep64 = slowPdep
#endif
{-# INLINE primPdep64 #-}

-- | Bitwise parallel deposit for 'Word32'.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> primPdep32 1 1
-- 1
primPdep32
  :: Word32 -- ^ word containing the bits that will be deposited
  -> Word32 -- ^ bitmap selecting the bits that are to be deposited
  -> Word32 -- ^ word containing the deposited bits
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPdep32 src mask =
  let !(W# src#)   = fromIntegral src  in
  let !(W# mask#)  = fromIntegral mask in
  fromIntegral (W# (pdep32# src# mask#))
#else
primPdep32 = slowPdep
#endif
{-# INLINE primPdep32 #-}

-- | Bitwise parallel deposit for 'Word16'.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> primPdep16 1 1
-- 1
primPdep16
  :: Word16 -- ^ word containing the bits that will be deposited
  -> Word16 -- ^ bitmap selecting the bits that are to be deposited
  -> Word16 -- ^ word containing the deposited bits
primPdep16 src mask = fromIntegral (primPdep32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep16 #-}

-- | Bitwise parallel deposit for 'Word8'.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> primPdep8 1 1
-- 1
primPdep8
  :: Word8 -- ^ word containing the bits that will be deposited
  -> Word8 -- ^ bitmap selecting the bits that are to be deposited
  -> Word8 -- ^ word containing the deposited bits
primPdep8 src mask = fromIntegral (primPdep32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep8 #-}

-- | Runtime flag indicating whether the 'pdep' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pdep` is emulated.
fastPdepEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPdepEnabled = True
#else
fastPdepEnabled = False
#endif
{-# INLINE fastPdepEnabled #-}
