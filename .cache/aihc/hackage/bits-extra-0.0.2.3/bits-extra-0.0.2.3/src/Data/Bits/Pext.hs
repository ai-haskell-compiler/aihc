{-# LANGUAGE CPP #-}

{-|
Module      : Data.Bits.Pext.Prim
Description : Parallel extract operations
Copyright   : (c) John Ky, 2018-2019
License     : BSD-3-Clause
Maintainer  : newhoggy@gmail.com
Stability   : stable
-}
module Data.Bits.Pext
  ( Pext(..)
  , fastPextEnabled
  ) where

import GHC.Word

import qualified Data.Bits.Pext.Prim as P

-- | Parallel extract bits for 'Word64'
--
-- Copies selected bits from 'src' to contiguous low-order bits of the return value;
-- higher-order return value bits are cleared.
--
-- >>> pext 1 1 :: Word64
-- 1
class Pext a where
  pext
    :: a -- ^ the bitmap from which bits will be extracted
    -> a -- ^ the bitmap selecting the bits that are to be extracted
    -> a -- ^ the bitmap containing the extract bits with higher-order bits cleared

instance Pext Word where
  pext = P.primPext
  {-# INLINE pext #-}

instance Pext Word8 where
  pext = P.primPext8
  {-# INLINE pext #-}

instance Pext Word16 where
  pext = P.primPext16
  {-# INLINE pext #-}

instance Pext Word32 where
  pext = P.primPext32
  {-# INLINE pext #-}

instance Pext Word64 where
  pext = P.primPext64
  {-# INLINE pext #-}

-- | Runtime flag indicating whether the 'pext' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pext` is emulated.
--
-- Actual performance when using the BMI2 instruction set will vary according to CPU
-- model.  For example Intel CPUs currently outperform AMD CPUs in this area.
fastPextEnabled :: Bool
fastPextEnabled = P.fastPextEnabled
{-# INLINE fastPextEnabled #-}
