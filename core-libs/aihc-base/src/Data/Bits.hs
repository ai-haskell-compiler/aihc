module Data.Bits
  ( Bits (..),
    FiniteBits (..),
    bitDefault,
    testBitDefault,
    popCountDefault,
    toIntegralSized,
    oneBits,
    (.^.),
    (.>>.),
    (.<<.),
    (!>>.),
    (!<<.),
    And (And),
    getAnd,
    Ior (Ior),
    getIor,
    Xor (Xor),
    getXor,
    Iff (Iff),
    getIff,
  )
where

import GHC.Bits
import Prelude (Eq (..), Int)

oneBits :: (FiniteBits a) => a
oneBits = complement zeroBits

(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 6 .^.

(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.

(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR

infixl 8 !>>.

(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL

infixl 8 !<<.

newtype And a = And a

getAnd :: And a -> a
getAnd (And value) = value

newtype Ior a = Ior a

getIor :: Ior a -> a
getIor (Ior value) = value

newtype Xor a = Xor a

getXor :: Xor a -> a
getXor (Xor value) = value

newtype Iff a = Iff a

getIff :: Iff a -> a
getIff (Iff value) = value

instance (Eq a) => Eq (And a) where
  And left == And right = left == right
  And left /= And right = left /= right

instance (Eq a) => Eq (Ior a) where
  Ior left == Ior right = left == right
  Ior left /= Ior right = left /= right

instance (Eq a) => Eq (Xor a) where
  Xor left == Xor right = left == right
  Xor left /= Xor right = left /= right

instance (Eq a) => Eq (Iff a) where
  Iff left == Iff right = left == right
  Iff left /= Iff right = left /= right

instance (Bits a) => Bits (And a) where
  And left .&. And right = And (left .&. right)
  And left .|. And right = And (left .|. right)
  xor (And left) (And right) = And (xor left right)
  complement (And value) = And (complement value)
  shift (And value) amount = And (shift value amount)
  rotate (And value) amount = And (rotate value amount)
  zeroBits = And zeroBits
  bit index = And (bit index)
  testBit (And value) = testBit value
  bitSizeMaybe (And value) = bitSizeMaybe value
  bitSize (And value) = bitSize value
  isSigned (And value) = isSigned value
  popCount (And value) = popCount value

instance (FiniteBits a) => FiniteBits (And a) where
  finiteBitSize (And value) = finiteBitSize value
  countLeadingZeros (And value) = countLeadingZeros value
  countTrailingZeros (And value) = countTrailingZeros value

instance (Bits a) => Bits (Ior a) where
  Ior left .&. Ior right = Ior (left .&. right)
  Ior left .|. Ior right = Ior (left .|. right)
  xor (Ior left) (Ior right) = Ior (xor left right)
  complement (Ior value) = Ior (complement value)
  shift (Ior value) amount = Ior (shift value amount)
  rotate (Ior value) amount = Ior (rotate value amount)
  zeroBits = Ior zeroBits
  bit index = Ior (bit index)
  testBit (Ior value) = testBit value
  bitSizeMaybe (Ior value) = bitSizeMaybe value
  bitSize (Ior value) = bitSize value
  isSigned (Ior value) = isSigned value
  popCount (Ior value) = popCount value

instance (FiniteBits a) => FiniteBits (Ior a) where
  finiteBitSize (Ior value) = finiteBitSize value
  countLeadingZeros (Ior value) = countLeadingZeros value
  countTrailingZeros (Ior value) = countTrailingZeros value

instance (Bits a) => Bits (Xor a) where
  Xor left .&. Xor right = Xor (left .&. right)
  Xor left .|. Xor right = Xor (left .|. right)
  xor (Xor left) (Xor right) = Xor (xor left right)
  complement (Xor value) = Xor (complement value)
  shift (Xor value) amount = Xor (shift value amount)
  rotate (Xor value) amount = Xor (rotate value amount)
  zeroBits = Xor zeroBits
  bit index = Xor (bit index)
  testBit (Xor value) = testBit value
  bitSizeMaybe (Xor value) = bitSizeMaybe value
  bitSize (Xor value) = bitSize value
  isSigned (Xor value) = isSigned value
  popCount (Xor value) = popCount value

instance (FiniteBits a) => FiniteBits (Xor a) where
  finiteBitSize (Xor value) = finiteBitSize value
  countLeadingZeros (Xor value) = countLeadingZeros value
  countTrailingZeros (Xor value) = countTrailingZeros value

instance (Bits a) => Bits (Iff a) where
  Iff left .&. Iff right = Iff (left .&. right)
  Iff left .|. Iff right = Iff (left .|. right)
  xor (Iff left) (Iff right) = Iff (xor left right)
  complement (Iff value) = Iff (complement value)
  shift (Iff value) amount = Iff (shift value amount)
  rotate (Iff value) amount = Iff (rotate value amount)
  zeroBits = Iff zeroBits
  bit index = Iff (bit index)
  testBit (Iff value) = testBit value
  bitSizeMaybe (Iff value) = bitSizeMaybe value
  bitSize (Iff value) = bitSize value
  isSigned (Iff value) = isSigned value
  popCount (Iff value) = popCount value

instance (FiniteBits a) => FiniteBits (Iff a) where
  finiteBitSize (Iff value) = finiteBitSize value
  countLeadingZeros (Iff value) = countLeadingZeros value
  countTrailingZeros (Iff value) = countTrailingZeros value
