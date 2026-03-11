{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Word64
  ( lsb
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Bits as DB

-- | Returns the position of the least significant bit (0-based).
--
-- This is equivalent to 'DB.countTrailingZeros' except for when there are no bits set.  In which case
-- return a word with all bits set.
--
-- >>> lsb 8
-- 3
-- >>> lsb 1
-- 0
-- >>> lsb 0
-- 18446744073709551615
lsb :: Word64 -> Word64
lsb w = let r = fromIntegral (DB.countTrailingZeros w) in r .|. negate ((r .>. 6) .&. 0x1)
{-# INLINE lsb #-}
