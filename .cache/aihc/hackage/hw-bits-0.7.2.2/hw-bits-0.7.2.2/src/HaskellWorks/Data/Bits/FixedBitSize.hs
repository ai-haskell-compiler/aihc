module HaskellWorks.Data.Bits.FixedBitSize
    ( FixedBitSize(..)
    ) where

import Data.Word
import HaskellWorks.Data.Positioning

-- | Class of values that have a fix bit size
class FixedBitSize a where
  -- | Get the bit size of a value of given type.
  --
  -- >>> fixedBitSize (undefined :: Word8)
  -- 8
  fixedBitSize :: a -> Count

instance FixedBitSize Bool where
  fixedBitSize _ = 1

instance FixedBitSize Word8 where
  fixedBitSize _ = 8

instance FixedBitSize Word16 where
  fixedBitSize _ = 16

instance FixedBitSize Word32 where
  fixedBitSize _ = 32

instance FixedBitSize Word64 where
  fixedBitSize _ = 64
