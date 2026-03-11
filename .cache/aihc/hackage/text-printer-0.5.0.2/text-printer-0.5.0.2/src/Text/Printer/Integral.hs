{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE BangPatterns #-}

-- | Print integral numbers in common positional numeral systems. 
module Text.Printer.Integral
  (
  -- * Positional systems
    PositionalSystem(..)
  , BitSystem(..)
  , Binary(..)
  , Octal(..)
  , Decimal(..)
  , Hexadecimal(..)
  , LowHex(..)
  , UpHex(..)
  -- * Numeral printers
  , nonNegative
  , nnBinary
  , nnOctal
  , nnDecimal
  , nnLowHex
  , nnUpHex
  , nnBits
  , nnBinaryBits
  , nnOctalBits
  , nnLowHexBits
  , nnUpHexBits
  , nonPositive
  , npBinary
  , npOctal
  , npDecimal
  , npLowHex
  , npUpHex
  , npBits
  , npBinaryBits
  , npOctalBits
  , npLowHexBits
  , npUpHexBits
  , number'
  , number
  , binary'
  , binary
  , octal'
  , octal
  , decimal'
  , decimal
  , lowHex'
  , lowHex
  , upHex'
  , upHex
  , bits'
  , bits
  , binaryBits'
  , binaryBits
  , octalBits'
  , octalBits
  , lowHexBits'
  , lowHexBits
  , upHexBits'
  , upHexBits
  ) where

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic)
#endif
import Data.Typeable (Typeable)
import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Data.Bits (Bits(..))
import Data.Monoid (mempty)
import qualified Text.Ascii as A
import Text.Printer

-- | Positional numeral system.
class PositionalSystem s where
  -- | The name of the system (e.g. \"binary\", \"decimal\").
  systemName ∷ s → String
  -- | The radix of the system.
  radixIn ∷ Num α ⇒ s → α
  -- | Test if a character is a digit.
  isDigitIn ∷ s → Char → Bool
  -- | Test if a character is a non-zero digit.
  isNzDigitIn ∷ s → Char → Bool
  -- | Map digits to the corresponding numbers. Return 'Nothing' on
  --   other inputs.
  fromDigitIn ∷ Num α ⇒ s → Char → Maybe α
  -- | Map non-zero digits to the corresponding numbers. Return 'Nothing' on
  --   other inputs.
  fromNzDigitIn ∷ Num α ⇒ s → Char → Maybe α
  -- | Map digits to the corresponding numbers. No checks are performed.
  unsafeFromDigitIn ∷ Num α ⇒ s → Char → α
  -- | Map 'Int' values to the corresponding digits. Inputs /must/ be
  --   non-negative and less than the radix.
  intToDigitIn ∷ s → Int → Char
  -- | Print a digit.
  printDigitIn ∷ Printer p ⇒ s → Char → p
  printDigitIn _ = char7
  {-# INLINE printDigitIn #-}
  printZeroIn ∷ Printer p ⇒ s → p
  printZeroIn s = printDigitIn s $! intToDigitIn s 0
  {-# INLINE printZeroIn #-}

-- | Positonal numeral system with a power of two radix.
class PositionalSystem s ⇒ BitSystem s where
  -- | Numer of bits occupied by a digit.
  digitBitsIn ∷ s → Int
  -- | The number that has 'digitBitsIn' least significant bits set to ones
  --   and all the other bits set to zeroes.
  digitMaskIn ∷ Num α ⇒ s → α
  -- | Map the last digit of a number to the corresponding 'Int' value.
  lastDigitIn ∷ Bits α ⇒ s → α → Int

-- | The binary numeral system.
data Binary = Binary deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                              , Generic
#endif
                              , Eq, Ord, Show, Read )

instance PositionalSystem Binary where
  systemName _ = "binary"
  {-# INLINE systemName #-}
  radixIn _ = 2
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isBinDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzBinDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromBinDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzBinDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromBinDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

instance BitSystem Binary where
  digitBitsIn _ = 1
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 1
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = if testBit n 0 then 1 else 0
  {-# INLINE lastDigitIn #-}

-- | The octal numeral system.
data Octal = Octal deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                            , Generic
#endif
                            , Eq, Ord, Show, Read )

instance PositionalSystem Octal where
  systemName _ = "octal"
  {-# INLINE systemName #-}
  radixIn _ = 8
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isOctDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzOctDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromOctDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzOctDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromOctDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

instance BitSystem Octal where
  digitBitsIn _ = 3
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 7
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
  {-# INLINE lastDigitIn #-}

-- | The decimal numeral system.
data Decimal = Decimal deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                , Generic
#endif
                                , Eq, Ord, Show, Read )

instance PositionalSystem Decimal where
  systemName _ = "decimal"
  {-# INLINE systemName #-}
  radixIn _ = 10
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isDecDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzDecDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromDecDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzDecDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromDecDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

-- | The hexadecimal numeral system.
data Hexadecimal = Hexadecimal deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                        , Generic
#endif
                                        , Eq, Ord, Show, Read )

instance PositionalSystem Hexadecimal where
  systemName _ = "hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

instance BitSystem Hexadecimal where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | The hexadecimal numeral system, using lower case digits.
data LowHex = LowHex deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                              , Generic
#endif
                              , Eq, Ord, Show, Read )

instance PositionalSystem LowHex where
  systemName _ = "lower case hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isLowHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzLowHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromLowHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzLowHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromLowHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'a' + (i - 10) 
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

instance BitSystem LowHex where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | The hexadecimal numeral system, using upper case digits.
data UpHex = UpHex deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                            , Generic
#endif
                            , Eq, Ord, Show, Read )

instance PositionalSystem UpHex where
  systemName _ = "upper case hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isUpHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzUpHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromUpHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzUpHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromUpHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

instance BitSystem UpHex where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | Print a non-negative number in the specified positional numeral system.
nonNegative ∷ (PositionalSystem s, Integral α, Printer p) ⇒ s → α → p
nonNegative s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ fromIntegral r
        radix = radixIn s
{-# INLINABLE nonNegative #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int8 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int16 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int32 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int64 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word8 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word16 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word32 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word64 → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Decimal → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-negative number in the binary numeral system.
nnBinary ∷ (Integral α, Printer p) ⇒ α → p
nnBinary = nonNegative Binary
{-# INLINE nnBinary #-}

-- | Print a non-negative number in the octal numeral system.
nnOctal ∷ (Integral α, Printer p) ⇒ α → p
nnOctal = nonNegative Octal
{-# INLINE nnOctal #-}

-- | Print a non-negative number in the decimal numeral system.
nnDecimal ∷ (Integral α, Printer p) ⇒ α → p
nnDecimal = nonNegative Decimal
{-# INLINE nnDecimal #-}

-- | Print a non-negative number in the hexadecimal numeral system
--   using lower case digits.
nnLowHex ∷ (Integral α, Printer p) ⇒ α → p
nnLowHex = nonNegative LowHex
{-# INLINE nnLowHex #-}

-- | Print a non-negative number in the hexadecimal numeral system
--   using upper case digits.
nnUpHex ∷ (Integral α, Printer p) ⇒ α → p
nnUpHex = nonNegative UpHex
{-# INLINE nnUpHex #-}

-- | Print a non-negative binary number in the specified positional numeral
--   system.
nnBits ∷ (BitSystem s, Num α, Bits α, Printer p) ⇒ s → α → p
nnBits s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        digitBits = digitBitsIn s
{-# INLINABLE nnBits #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Hexadecimal → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-negative binary number in the binary numeral system.
nnBinaryBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnBinaryBits = nnBits Binary
{-# INLINE nnBinaryBits #-}

-- | Print a non-negative binary number in the octal numeral system.
nnOctalBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnOctalBits = nnBits Octal
{-# INLINE nnOctalBits #-}

-- | Print a non-negative binary number in the hexadecimal numeral system
--   using lower case digits.
nnLowHexBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnLowHexBits = nnBits LowHex
{-# INLINE nnLowHexBits #-}

-- | Print a non-negative binary number in the hexadecimal numeral system
--   using upper case digits.
nnUpHexBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnUpHexBits = nnBits UpHex
{-# INLINE nnUpHexBits #-}

-- | Print a non-positive number in the specified positional numeral system.
--   For example, @'nonPositive' 'Decimal' (-/123/)@ would print \"123\".
nonPositive ∷ (PositionalSystem s, Integral α, Printer p) ⇒ s → α → p
nonPositive s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ negate $ fromIntegral r
        radix = radixIn s
{-# INLINABLE nonPositive #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int8 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int16 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int32 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int64 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word8 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word16 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word32 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word64 → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Decimal → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-positive number in the binary numeral system.
npBinary ∷ (Integral α, Printer p) ⇒ α → p
npBinary = nonPositive Binary
{-# INLINE npBinary #-}

-- | Print a non-positive number in the octal numeral system.
npOctal ∷ (Integral α, Printer p) ⇒ α → p
npOctal = nonPositive Octal
{-# INLINE npOctal #-}

-- | Print a non-positive number in the decimal numeral system.
npDecimal ∷ (Integral α, Printer p) ⇒ α → p
npDecimal = nonPositive Decimal
{-# INLINE npDecimal #-}

-- | Print a non-positive number in the hexadecimal numeral system
--   using lower case digits.
npLowHex ∷ (Integral α, Printer p) ⇒ α → p
npLowHex = nonPositive LowHex
{-# INLINE npLowHex #-}

-- | Print a non-positive number in the hexadecimal numeral system
--   using upper case digits.
npUpHex ∷ (Integral α, Printer p) ⇒ α → p
npUpHex = nonPositive UpHex
{-# INLINE npUpHex #-}

-- | Print a non-positive two-compliment binary number in the specified
--   positional numeral system. For example, @'npBits' 'UpHex' (-/0xABC/)@
--   would print \"ABC\".
npBits ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p) ⇒ s → α → p
npBits s n = case testBit n 0 of
    True → go mempty (shiftR (negate n) digitBits) <> printDigitIn s d
      where !d = intToDigitIn s $ radix - lastDigitIn s n
    False → case n > negRadix of
        True → printDigitIn s d'
        False → go mempty m <> printDigitIn s d'
          where m | d == 0    = negate $ shiftR n digitBits
                  | otherwise = complement $ shiftR n digitBits
      where !d  = lastDigitIn s n
            !d' = intToDigitIn s $ (radix - d) .&. digitMask
  where go p 0 = p
        go p m = go p (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        radix     = radixIn s
        digitMask = digitMaskIn s
        digitBits = digitBitsIn s
        negRadix  = complement $ digitMaskIn s
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word64 → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Hexadecimal → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-positive binary number in the binary numeral system.
npBinaryBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npBinaryBits = npBits Binary
{-# INLINE npBinaryBits #-}

-- | Print a non-positive binary number in the octal numeral system.
npOctalBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npOctalBits = npBits Octal
{-# INLINE npOctalBits #-}

-- | Print a non-positive binary number in the hexadecimal numeral system
--   using lower case digits.
npLowHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npLowHexBits = npBits LowHex
{-# INLINE npLowHexBits #-}

-- | Print a non-positive binary number in the hexadecimal numeral system
--   using upper case digits.
npUpHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npUpHexBits = npBits UpHex
{-# INLINE npUpHexBits #-}

-- | Print a number in the specified positional numeral system.
number' ∷ (PositionalSystem s, Integral α, Printer p)
        ⇒ s
        → p -- ^ Prefix for negative values
        → p -- ^ Zero printer
        → p -- ^ Prefix for positive values
        → α → p
number' s neg z pos n = case compare n 0 of
    LT → go neg q <> printDigitIn s d
      where (q, r) = quotRem n (negate radix)
            !d     = intToDigitIn s $ negate $ fromIntegral r
    EQ → z
    GT → go pos q <> printDigitIn s d
      where (q, r) = quotRem n radix
            !d     = intToDigitIn s $ fromIntegral r
  where go p 0 = p
        go p m = go p q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ fromIntegral r
        radix = radixIn s
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int8 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int16 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int32 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int64 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word8 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word16 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word32 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word64 → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ Binary → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ Decimal → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ Octal → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ Hexadecimal → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ LowHex → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Integral α, Printer p) ⇒ UpHex → p → p → p → α → p #-}

-- | Print a number in the specified positional numeral system. Negative
--   values are prefixed with a minus sign.
number ∷ (PositionalSystem s, Integral α, Printer p) ⇒ s → α → p
number s = number' s (char7 '-') (printZeroIn s) mempty
{-# INLINE number #-}

-- | Print a number in the binary numeral system.
binary' ∷ (Integral α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Zero printer
        → p -- ^ Prefix for positive values
        → α → p
binary' = number' Binary
{-# INLINE binary' #-}

-- | Print a number in the binary numeral system. Negative values
--   are prefixed with a minus sign.
binary ∷ (Integral α, Printer p) ⇒ α → p
binary = number Binary
{-# INLINE binary #-}

-- | Print a number in the octal numeral system.
octal' ∷ (Integral α, Printer p)
       ⇒ p -- ^ Prefix for negative values
       → p -- ^ Zero printer
       → p -- ^ Prefix for positive values
       → α → p
octal' = number' Octal
{-# INLINE octal' #-}

-- | Print a number in the octal numeral system. Negative values
--   are prefixed with a minus sign.
octal ∷ (Integral α, Printer p) ⇒ α → p
octal = number Octal
{-# INLINE octal #-}

-- | Print a number in the decimal numeral system.
decimal' ∷ (Integral α, Printer p)
         ⇒ p -- ^ Prefix for negative values
         → p -- ^ Zero printer
         → p -- ^ Prefix for positive values
         → α → p
decimal' = number' Decimal
{-# INLINE decimal' #-}

-- | Print a number in the decimal numeral system. Negative values
--   are prefixed with a minus sign.
decimal ∷ (Integral α, Printer p) ⇒ α → p
decimal = number Decimal
{-# INLINE decimal #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits.
lowHex' ∷ (Integral α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Zero printer
        → p -- ^ Prefix for positive values
        → α → p
lowHex' = number' LowHex
{-# INLINE lowHex' #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits. Negative values are prefixed with a minus sign.
lowHex ∷ (Integral α, Printer p) ⇒ α → p
lowHex = number LowHex
{-# INLINE lowHex #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits.
upHex' ∷ (Integral α, Printer p)
       ⇒ p -- ^ Prefix for negative values
       → p -- ^ Zero printer
       → p -- ^ Prefix for positive values
       → α → p
upHex' = number' UpHex
{-# INLINE upHex' #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits. Negative values are prefixed with a minus sign.
upHex ∷ (Integral α, Printer p) ⇒ α → p
upHex = number UpHex
{-# INLINE upHex #-}

-- | Print a binary number in the specified positional numeral system.
bits' ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p)
      ⇒ s
      → p -- ^ Prefix for negative values
      → p -- ^ Zero printer
      → p -- ^ Prefix for positive values
      → α → p
bits' s neg z pos n = case compare n 0 of
    LT → case testBit n 0 of
           True → go neg (shiftR (negate n) digitBits) <> printDigitIn s d
             where !d = intToDigitIn s $ radix - lastDigitIn s n
           False → case n > negRadix of
               True → neg <> printDigitIn s d'
               False → go neg m <> printDigitIn s d'
                 where m | d == 0    = negate $ shiftR n digitBits
                         | otherwise = complement $ shiftR n digitBits
             where !d  = lastDigitIn s n
                   !d' = intToDigitIn s $ (radix - d) .&. digitMask
    EQ → z
    GT → go pos (shiftR n digitBits) <> printDigitIn s d
      where !d = intToDigitIn s $ lastDigitIn s n
  where go p 0 = p
        go p m = go p (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        radix     = radixIn s
        digitMask = digitMaskIn s
        digitBits = digitBitsIn s
        negRadix  = complement $ digitMaskIn s
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Binary → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Octal → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Hexadecimal → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ LowHex → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ UpHex → p → p → p → α → p #-}

-- | Print a binary number in the specified positional numeral system.
--   Negative values are prefixed with a minus sign.
bits ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p) ⇒ s → α → p
bits s = bits' s (char7 '-') (printZeroIn s) mempty
{-# INLINE bits #-}

-- | Print a binary number in the binary numeral system.
binaryBits' ∷ (Ord α, Num α, Bits α, Printer p)
            ⇒ p -- ^ Prefix for negative values
            → p -- ^ Zero printer
            → p -- ^ Prefix for positive values
            → α → p
binaryBits' = bits' Binary
{-# INLINE binaryBits' #-}

-- | Print a binary number in the binary numeral system. Negative values
--   are prefixed with a minus sign.
binaryBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
binaryBits = bits Binary
{-# INLINE binaryBits #-}

-- | Print a binary number in the octal numeral system.
octalBits' ∷ (Ord α, Num α, Bits α, Printer p)
           ⇒ p -- ^ Prefix for negative values
           → p -- ^ Zero printer
           → p -- ^ Prefix for positive values
           → α → p
octalBits' = bits' Octal
{-# INLINE octalBits' #-}

-- | Print a binary number in the octal numeral system. Negative values
--   are prefixed with a minus sign.
octalBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
octalBits = bits Octal
{-# INLINE octalBits #-}

-- | Print a binary number in the hexadecimal numeral system using lower
--   case digits.
lowHexBits' ∷ (Ord α, Num α, Bits α, Printer p)
            ⇒ p -- ^ Prefix for negative values
            → p -- ^ Zero printer
            → p -- ^ Prefix for positive values
            → α → p
lowHexBits' = bits' LowHex
{-# INLINE lowHexBits' #-}

-- | Print a binary number in the hexadecimal numeral system using lower
--   case digits. Negative values are prefixed with a minus sign.
lowHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
lowHexBits = bits LowHex
{-# INLINE lowHexBits #-}

-- | Print a binary number in the hexadecimal numeral system using upper
--   case digits.
upHexBits' ∷ (Ord α, Num α, Bits α, Printer p)
           ⇒ p -- ^ Prefix for negative values
           → p -- ^ Zero printer
           → p -- ^ Prefix for positive values
           → α → p
upHexBits' = bits' UpHex
{-# INLINE upHexBits' #-}

-- | Print a binary number in the hexadecimal numeral system using upper
--   case digits. Negative values are prefixed with a minus sign.
upHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
upHexBits = bits UpHex
{-# INLINE upHexBits #-}
