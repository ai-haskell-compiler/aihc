{-# LANGUAGE CPP #-}

{-|
Module      : Data.Bits.Pdep.Prim
Description : Parallel deposit operations
Copyright   : (c) John Ky, 2018-2019
License     : BSD-3-Clause
Maintainer  : newhoggy@gmail.com
Stability   : stable
-}
module Data.Bits.Pdep
  ( Pdep(..)
  , fastPdepEnabled
  ) where

import GHC.Word

import qualified Data.Bits.Pdep.Prim as P

-- | Bitwise parallel deposit.  Deposits bits from the source at the locations
-- described by the mask.
--
-- Copies lower order bits from 'src' to 'mask' 1-bit locations in the return value;
-- 'mask' 0-bit locations in the return value will be cleared.
--
-- >>> pdep 1 1 :: Word64
-- 1
class Pdep a where
  pdep
    :: a -- ^ the bitmap from which bits will be extracted
    -> a -- ^ the bitmap selecting the bit locations that are to be deposited to
    -> a -- ^ the bitmap containing the deposited bits with other bits bits cleared

instance Pdep Word where
  pdep = P.primPdep
  {-# INLINE pdep #-}

instance Pdep Word8 where
  pdep = P.primPdep8
  {-# INLINE pdep #-}

instance Pdep Word16 where
  pdep = P.primPdep16
  {-# INLINE pdep #-}

instance Pdep Word32 where
  pdep = P.primPdep32
  {-# INLINE pdep #-}

instance Pdep Word64 where
  pdep = P.primPdep64
  {-# INLINE pdep #-}

-- | Runtime flag indicating whether the 'pdep' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pdep` is emulated.
--
-- Actual performance when using the BMI2 instruction set will vary according to CPU
-- model.  For example Intel CPUs currently outperform AMD CPUs in this area.
fastPdepEnabled :: Bool
fastPdepEnabled = P.fastPdepEnabled
{-# INLINE fastPdepEnabled #-}
