{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.BitWise
    ( -- * Bit map
      BitWise(..)
    , Bit(..)
    , Shift(..)
    , TestBit(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning
import Prelude                          as P

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Bits            as B
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

-- | Class of values that have shift operations
class Shift a where
  -- | Shift left by the specified count
  (.<.) :: a -> Count -> a
  -- | Shift right by the specified count
  (.>.) :: a -> Count -> a

  -- | Class of values that have bit test operations
class TestBit a where
  -- | Test whether the bit ad the given offset is set
  (.?.) :: a -> Position -> Bool

-- | Class of values that have bit wise logical operations
class BitWise a where
  -- | Bit-wise AND
  (.&.) :: a -> a -> a
  -- | Bit-wise OR
  (.|.) :: a -> a -> a
  -- | Bit-wise XOR
  (.^.) :: a -> a -> a
  -- | Bit-wise complement
  comp  :: a -> a
  -- | Bit-wise value of the given type with all bits set to zero
  all0s :: a
  -- | Bit-wise value of the given type with all bits set to one
  all1s :: a

class Bit a where
  bit :: Position -> a

--------------------------------------------------------------------------------
-- Instances

instance TestBit Bool where
  (.?.) w 0 = w
  (.?.) _ _ = error "Invalid bit index"
  {-# INLINE (.?.) #-}

instance TestBit [Bool] where
  (.?.) v p = v !! fromIntegral p
  {-# INLINE (.?.) #-}

instance TestBit Int where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word8) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word16) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word32) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word64) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVU.Vector Bit.Bit) where
  (.?.) v n = Bit.unBit (v DVU.! fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (DVU.Vector BitTS.Bit) where
  (.?.) v n = BitTS.unBit (v DVU.! fromIntegral n)
  {-# INLINE (.?.) #-}

instance BitWise Int where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = -1
  {-# INLINE all1s #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xff
  {-# INLINE all1s #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffff
  {-# INLINE all1s #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffff
  {-# INLINE all1s #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffffffffffff
  {-# INLINE all1s #-}

instance Shift Int  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Int8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Int16  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Int32  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Int64  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

-----

instance Bit Bool where
  bit 1 = True
  bit _ = False
  {-# INLINE bit #-}

instance Bit Int where
  bit n = 1 .<. toCount n
  {-# INLINE bit #-}

instance Bit Word8 where
  bit n = 1 .<. toCount n
  {-# INLINE bit #-}

instance Bit Word16 where
  bit n = 1 .<. toCount n
  {-# INLINE bit #-}

instance Bit Word32 where
  bit n = 1 .<. toCount n
  {-# INLINE bit #-}

instance Bit Word64 where
  bit n = 1 .<. toCount n
  {-# INLINE bit #-}

instance Bit (Naive Word8) where
  bit n = Naive (bit n)
  {-# INLINE bit #-}

instance Bit (Naive Word16) where
  bit n = Naive (bit n)
  {-# INLINE bit #-}

instance Bit (Naive Word32) where
  bit n = Naive (bit n)
  {-# INLINE bit #-}

instance Bit (Naive Word64) where
  bit n = Naive (bit n)
  {-# INLINE bit #-}
