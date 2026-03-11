{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsers for integral numbers written in positional numeral systems.
module Data.Textual.Integral
  (
  -- * Positional numeral systems
    PositionalSystem(..)
  , BitSystem(..)
  , Binary(..)
  , Octal(..)
  , Decimal(..)
  , Hexadecimal(..)
  , LowHex(..)
  , UpHex(..)
  -- * Single digits
  , digitIn
  , nzDigitIn
  , binDigit
  , nzBinDigit
  , octDigit
  , nzOctDigit
  , decDigit
  , nzDecDigit
  , hexDigit
  , nzHexDigit
  , lowHexDigit
  , nzLowHexDigit
  , upHexDigit
  , nzUpHexDigit
  -- * Numbers
  , nonNegative
  , nnCompact
  , nnUpTo
  , nncUpTo
  , nnBounded
  , nncBounded
  , nnBits
  , nncBits
  , nnBitsUpTo
  , nncBitsUpTo
  , nnbBits
  , nncbBits
  , nonPositive
  , npCompact
  , npUpTo
  , npcUpTo
  , npBounded
  , npcBounded
  , npBits
  , npcBits
  , npBitsUpTo
  , npcBitsUpTo
  , npbBits
  , npcbBits
  , Sign(..)
  , applySign
  , optMinus
  , optSign
  , number'
  , number
  , compact'
  , compact
  , numberUpTo'
  , numberUpTo
  , compactUpTo'
  , compactUpTo
  , bounded'
  , bounded
  , cBounded'
  , cBounded
  , bits'
  , bits
  , cBits'
  , cBits
  , bitsUpTo'
  , bitsUpTo
  , cBitsUpTo'
  , cBitsUpTo
  , bBits'
  , bBits
  , cbBits'
  , cbBits
  ) where

import Data.Typeable (Typeable)
import Data.Int
import Data.Word
import Data.Bits (Bits(..))
import Control.Applicative
import Text.Printer.Integral (
         PositionalSystem(..), BitSystem(..),
         Binary(..), Octal(..), Decimal(..), Hexadecimal(..),
         LowHex(..), UpHex(..))
import Text.Parser.Combinators ((<?>))
import qualified Text.Parser.Combinators as PC
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC

-- | Parse a digit of the specified positional numeral system.
digitIn ∷ (PositionalSystem s, Num α, CharParsing μ) ⇒ s → μ α
digitIn s  =  unsafeFromDigitIn s <$> PC.satisfy (isDigitIn s)
          <?> systemName s ++ " digit"
{-# INLINE digitIn #-}

-- | Parse a non-zero digit of the specified positional numeral system.
nzDigitIn ∷ (PositionalSystem s, Num α, CharParsing μ) ⇒ s → μ α
nzDigitIn s  =  unsafeFromDigitIn s <$> PC.satisfy (isNzDigitIn s)
            <?> "non-zero " ++ systemName s ++ " digit"
{-# INLINE nzDigitIn #-}

-- | Parse a binary digit.
binDigit ∷ (Num α, CharParsing μ) ⇒ μ α
binDigit = digitIn Binary
{-# INLINE binDigit #-}

-- | Parse a non-zero binary digit (/'1'/).
nzBinDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzBinDigit = nzDigitIn Binary
{-# INLINE nzBinDigit #-}

-- | Parse a decimal digit.
decDigit ∷ (Num α, CharParsing μ) ⇒ μ α
decDigit = digitIn Decimal
{-# INLINE decDigit #-}

-- | Parse a non-zero decimal digit.
nzDecDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzDecDigit = nzDigitIn Decimal
{-# INLINE nzDecDigit #-}

-- | Parse an octal digit.
octDigit ∷ (Num α, CharParsing μ) ⇒ μ α
octDigit = digitIn Octal
{-# INLINE octDigit #-}

-- | Parse a non-zero octal digit.
nzOctDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzOctDigit = nzDigitIn Octal
{-# INLINE nzOctDigit #-}

-- | Parse a hexadecimal digit.
hexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
hexDigit = digitIn Hexadecimal
{-# INLINE hexDigit #-}

-- | Parse a non-zero hexadecimal digit.
nzHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzHexDigit = nzDigitIn Hexadecimal
{-# INLINE nzHexDigit #-}

-- | Parse a lower case hexadecimal digit.
lowHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
lowHexDigit = digitIn LowHex
{-# INLINE lowHexDigit #-}

-- | Parse a non-zero lower case hexadecimal digit.
nzLowHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzLowHexDigit = nzDigitIn LowHex
{-# INLINE nzLowHexDigit #-}

-- | Parse an upper case hexadecimal digit.
upHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
upHexDigit = digitIn UpHex
{-# INLINE upHexDigit #-}

-- | Parse a non-zero upper case hexadecimal digit.
nzUpHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzUpHexDigit = nzDigitIn UpHex
{-# INLINE nzUpHexDigit #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system.
nonNegative ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nonNegative s = digit >>= go <?> systemName s ++ " digits"
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix + d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE nonNegative ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nonNegative ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system. Leading zeroes are not allowed.
nnCompact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nnCompact s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go $ fromIntegral (r ∷ Word)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix + d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE nnCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nnCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

moreThan ∷ CharParsing μ ⇒ Int → μ α
moreThan n = PC.unexpected
           $ "more than " ++ show n ++
             case n of
               1 → " digit"
               _ → " digits"
{-# INLINE moreThan #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system (up to /n/ digits).
nnUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
nnUpTo _ n | n <= 0 = empty
nnUpTo s n = digit >>= go (n - 1) <?> systemName s ++ " digits"
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix + d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int8 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int16 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int32 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int64 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word8 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word16 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word32 #-}
{-# SPECIALIZE nnUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word64 #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → Int → μ α #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE nnUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system (up to /n/ digits). Leading zeroes are not allowed.
nncUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
nncUpTo _ n | n <= 0 = empty
nncUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go (n - 1) $ fromIntegral (r ∷ Word)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix + d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int8 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int16 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int32 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int64 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word8 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word16 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word32 #-}
{-# SPECIALIZE nncUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word64 #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → Int → μ α #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE nncUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system, failing on overflow.
nnBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ) ⇒ s → μ α
nnBounded s = digit >>= go <?> systemName s ++ " digits"
  where (q, r) = quotRem maxBound radix
        go !n  = optional digit >>= \case
                   Just n1 → if n < q || (n == q && n1 <= r)
                             then go (n * radix + n1)
                             else PC.unexpected "out of bounds"
                   Nothing → return n
        radix  = radixIn s
        digit  = digitIn s
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE nnBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nnBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system, failing on overflow. Leading zeroes are not allowed.
nncBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
              Monad μ, CharParsing μ) ⇒ s → μ α
nncBounded s = (<?> systemName s ++ " digits") $ digit >>= \case
                 0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                       Just _  → PC.unexpected "leading zero"
                       Nothing → return 0
                 n → go n
  where (q, r) = quotRem maxBound radix
        go !n  = optional digit >>= \case
                   Just d  → if n < q || (n == q && d <= r)
                             then go (n * radix + d)
                             else PC.unexpected "out of bounds"
                   Nothing → return n
        radix  = radixIn s
        digit  = digitIn s
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE nncBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nncBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system.
nnBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
nnBits s = digit >>= go <?> systemName s ++ " digits"
  where go !r     = optional digit >>= \case
                      Just d  → go ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE nnBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system. Leading zeroes are not allowed.
nncBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
nncBits s = (<?> systemName s ++ " digits") $ digit >>= \case
              0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → PC.unexpected "leading zero"
                    Nothing → return 0 
              r → go r
  where go !r     = optional digit >>= \case
                      Just d  → go ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE nncBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE nncBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nncBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nncBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nncBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nncBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system (up to /n/ digits).
nnBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
nnBitsUpTo _ n | n <= 0 = empty
nnBitsUpTo s n = digit >>= go (n - 1) <?> systemName s ++ " digits"
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d  → go (l - 1) ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word8 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word16 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word32 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word64 #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE nnBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system (up to /n/ digits). Leading zeroes are not
--   allowed.
nncBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
            ⇒ s → Int → μ α
nncBitsUpTo _ n | n <= 0 = empty
nncBitsUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                    0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                          Just _  → PC.unexpected "leading zero"
                          Nothing → return 0 
                    r → go (n - 1) $ fromIntegral (r ∷ Word)
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d  → go (l - 1) ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word8 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word16 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word32 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word64 #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE nncBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system, failing on overflow.
nnbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ s → μ α
nnbBits s = digit >>= go <?> systemName s ++ " digits"
  where q = maxBound `shiftR` digitBits
        r = maxBound .&. digitMaskIn s
        go !n = optional digit >>= \case
                  Just d  → if n < q || (n == q && d <= r)
                            then go ((n `shiftL` digitBits) .|. d)
                            else PC.unexpected "out of bounds"
                  Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE nnbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE nnbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nnbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nnbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nnbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nnbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-negative binary number written in the specified
--   positional numeral system, failing on overflow. Leading zeroes are not
--   allowed.
nncbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
            Monad μ, CharParsing μ)
         ⇒ s → μ α
nncbBits s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
               0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                     Just _  → PC.unexpected "leading zero"
                     Nothing → return 0
               n → go $ fromIntegral (n ∷ Word)
  where q = maxBound `shiftR` digitBits
        r = maxBound .&. digitMaskIn s
        go !n = optional digit >>= \case
                  Just d  → if n < q || (n == q && d <= r)
                            then go ((n `shiftL` digitBits) .|. d)
                            else PC.unexpected "out of bounds"
                  Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE nncbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE nncbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nncbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nncbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nncbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nncbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system. For example, parsing \"123\" as a decimal would produce
--   /-123/, not /123/.
nonPositive ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nonPositive s = (<?> systemName s ++ " digits") $ do
                  r ← digitIn s
                  go $ fromIntegral $ negate (r ∷ Int)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix - d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE nonPositive ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE nonPositive ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system. Leading zeroes are not allowed.
npCompact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
npCompact s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go $ fromIntegral $ negate (r ∷ Int)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix - d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE npCompact ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npCompact ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system (up to /n/ digits).
npUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
npUpTo _ n | n <= 0 = empty
npUpTo s n = (<?> systemName s ++ " digits") $ do
               r ← digitIn s
               go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix - d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int8 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int16 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int32 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int64 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word8 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word16 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word32 #-}
{-# SPECIALIZE npUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word64 #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → Int → μ α #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE npUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system (up to /n/ digits). Leading zeroes are not allowed.
npcUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
npcUpTo _ n | n <= 0 = empty
npcUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix - d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int8 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int16 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int32 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Int64 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word8 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word16 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word32 #-}
{-# SPECIALIZE npcUpTo ∷ (Monad μ, CharParsing μ) ⇒ Decimal → Int → μ Word64 #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Decimal → Int → μ α #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE npcUpTo ∷ (Num α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system, failing on overflow.
npBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ)
          ⇒ s → μ α
npBounded s = (<?> systemName s ++ " digits") $ do
                n ← digitIn s
                go $ fromIntegral $ negate (n ∷ Int)
  where (q, r1) = quotRem minBound radix
        !r      = negate r1
        go !n   = optional digit >>= \case
                    Just d  → if n > q || (n == q && d <= r)
                              then go (n * radix - d)
                              else PC.unexpected "out of bounds"
                    Nothing → return n
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE npBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive number written in the specified positional
--   numeral system, failing on overflow. Leading zeroes are not allowed.
npcBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
              Monad μ, CharParsing μ)
           ⇒ s → μ α
npcBounded s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                 0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                       Just _  → PC.unexpected "leading zero"
                       Nothing → return 0
                 n → go $ fromIntegral $ negate (n ∷ Int)
  where (q, r1) = quotRem minBound radix
        !r      = negate r1
        go !n   = optional digit >>= \case
                    Just d  → if n > q || (n == q && d <= r)
                              then go (n * radix - d)
                              else PC.unexpected "out of bounds"
                    Nothing → return n
        radix   = radixIn s
        digit   = digitIn s
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int8 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int16 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int32 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Int64 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word8 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word16 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word32 #-}
{-# SPECIALIZE npcBounded ∷ (Monad μ, CharParsing μ) ⇒ Decimal → μ Word64 #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Decimal → μ α #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npcBounded ∷ (Bounded α, Integral α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system.
npBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
npBits s = (<?> systemName s ++ " digits") $ do
             r ← digit
             go $ fromIntegral $ negate (r ∷ Int)
  where go !r     = optional digit >>= \case
                      Just d1 → go ((r `shiftL` digitBits) + d)
                        where !d = fromIntegral $ negate d1
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE npBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE npBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system (up to /n/ digits).
npBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
npBitsUpTo _ n | n <= 0 = empty
npBitsUpTo s n = (<?> systemName s ++ " digits") $ do
                   r ← digit
                   go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d1 → go (l - 1) ((r `shiftL` digitBits) + d)
                        where !d = fromIntegral $ negate d1
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word8 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word16 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word32 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word64 #-}
{-# SPECIALIZE npBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE npBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE npBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE npBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE npBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system. Leading zeroes are not allowed.
npcBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
npcBits s = (<?> systemName s ++ " digits") $ digit >>= \case
              0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → PC.unexpected "leading zero"
                    Nothing → return 0
              r → go $ fromIntegral $ negate (r ∷ Int)
  where go !r     = optional digit >>= \case
                      Just d1 → go ((r `shiftL` digitBits) + d)
                        where !d = fromIntegral $ negate d1
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE npcBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE npcBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npcBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npcBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npcBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npcBits ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system (up to /n/ digits).
--   Leading zeroes are not allowed.
npcBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
            ⇒ s → Int → μ α
npcBitsUpTo _ n | n <= 0 = empty
npcBitsUpTo s n = (<?> systemName s ++ " digits") $ digit >>= \case
                    0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                          Just _  → PC.unexpected "leading zero"
                          Nothing → return 0
                    r → go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d1 → go (l - 1) ((r `shiftL` digitBits) + d)
                        where !d = fromIntegral $ negate d1
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Int64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Binary → Int → μ Word64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Int64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Octal → Int → μ Word64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Int64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ Word64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Int64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ LowHex → Int → μ Word64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Int64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word8 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word16 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word32 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Monad μ, CharParsing μ) ⇒ UpHex → Int → μ Word64 #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → Int → μ α #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → Int → μ α #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → Int → μ α #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → Int → μ α #-}
{-# SPECIALIZE npcBitsUpTo ∷ (Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → Int → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system, failing on overflow.
npbBits ∷ ∀ s μ α
        . (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ s → μ α
npbBits s = (<?> systemName s ++ " digits") $ do
              n ← digit
              go $ fromIntegral $ negate (n ∷ Int)
  where q1 = minBound `shiftR` digitBits
        r  = negate (lastDigitIn s (minBound ∷ α)) .&. digitMaskIn s
        q  = if r == 0 then q1 else q1 + 1
        go !n     = optional digit >>= \case
                      Just d1 → if n > q || (n == q && d1 <= r)
                                then go ((n `shiftL` digitBits) + d)
                                else PC.unexpected "out of bounds"
                        where !d = fromIntegral $ negate d1
                      Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE npbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE npbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system, failing on overflow.
--   Leading zeroes are not allowed.
npcbBits ∷ ∀ s μ α
         . (BitSystem s, Ord α, Bounded α, Num α, Bits α,
            Monad μ, CharParsing μ)
        ⇒ s → μ α
npcbBits s = (<?> systemName s ++ " digits") $ digit >>= \case
               0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                     Just _  → PC.unexpected "leading zero"
                     Nothing → return 0
               n → go $ fromIntegral $ negate (n ∷ Int)
  where q1 = minBound `shiftR` digitBits
        r  = negate (lastDigitIn s (minBound ∷ α)) .&. digitMaskIn s
        q  = if r == 0 then q1 else q1 + 1
        go !n     = optional digit >>= \case
                      Just d1 → if n > q || (n == q && d1 <= r)
                                then go ((n `shiftL` digitBits) + d)
                                else PC.unexpected "out of bounds"
                        where !d = fromIntegral $ negate d1
                      Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Int64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Binary → μ Word64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Int64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Octal → μ Word64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Int64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ Hexadecimal → μ Word64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Int64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ LowHex → μ Word64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Int64 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word8 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word16 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word32 #-}
{-# SPECIALIZE npcbBits ∷ (Monad μ, CharParsing μ) ⇒ UpHex → μ Word64 #-}
{-# SPECIALIZE npcbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Binary → μ α #-}
{-# SPECIALIZE npcbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Octal → μ α #-}
{-# SPECIALIZE npcbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ Hexadecimal → μ α #-}
{-# SPECIALIZE npcbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ LowHex → μ α #-}
{-# SPECIALIZE npcbBits ∷ (Ord α, Bounded α, Num α, Bits α, Monad μ, CharParsing μ) ⇒ UpHex → μ α #-}

-- | Sign of a number.
data Sign = NonNegative | NonPositive
            deriving (Typeable, Eq, Show, Read)

-- | Negate the supplied value if the sign is 'NonPositive' and return it
--   as it is otherwise.
applySign ∷ Num α ⇒ Sign → α → α
applySign NonNegative a = a
applySign NonPositive a = negate a

-- | Optional minus sign.
optMinus ∷ CharParsing μ ⇒ μ Sign
optMinus  =  (pure NonPositive <* PC.char '-') <|> pure NonNegative
         <?> "optional minus sign"
{-# INLINE optMinus #-}

-- | Optional minus or plus sign.
optSign ∷ CharParsing μ ⇒ μ Sign
optSign  =  (pure NonPositive <* PC.char '-')
        <|> (pure NonNegative <* PC.char '+')
        <|> pure NonNegative
        <?> "optional sign"
{-# INLINABLE optSign #-}

-- | Parse a number written in the specified positional numeral system.
--   The supplied parser is used to determine the sign of the number.
number' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
        ⇒ μ Sign → s → μ α
number' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nonNegative s
  NonPositive → nonPositive s
{-# INLINE number' #-}

-- | A shorthand for 'number'' 'optMinus'.
number ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
number = number' optMinus
{-# INLINE number #-}

-- | Parse a number written in the specified positional numeral system.
--   The supplied parser is used to determine the sign of the number.
--   Leading zeroes are not allowed.
compact' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
         ⇒ μ Sign → s → μ α
compact' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnCompact s
  NonPositive → npCompact s
{-# INLINE compact' #-}

-- | A shorthand for 'compact'' 'optMinus'.
compact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
compact = compact' optMinus
{-# INLINE compact #-}

-- | Parse a number written in the specified positional numeral system
--   (up to /n/ digits). The supplied parser is used to determine the sign of
--   the number.
numberUpTo' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
            ⇒ μ Sign → s → Int → μ α
numberUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nnUpTo s n
  NonPositive → npUpTo s n
{-# INLINE numberUpTo' #-}

-- | A shorthand for 'numberUpTo'' 'optMinus'.
numberUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
numberUpTo = numberUpTo' optMinus
{-# INLINE numberUpTo #-}

-- | Parse a number written in the specified positional numeral system
--   (up to /n/ digits). The supplied parser is used to determine the sign of
--   the number. Leading zeroes are not allowed.
compactUpTo' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
             ⇒ μ Sign → s → Int → μ α
compactUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nncUpTo s n
  NonPositive → npcUpTo s n
{-# INLINE compactUpTo' #-}

-- | A shorthand for 'compactUpTo'' 'optMinus'.
compactUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
compactUpTo = compactUpTo' optMinus
{-# INLINE compactUpTo #-}

-- | Parse a number written in the specified positional numeral system,
--   failing on overflow. The supplied parser is used to determine the sign
--   of the number.
bounded' ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
            Monad μ, CharParsing μ)
         ⇒ μ Sign → s → μ α
bounded' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBounded s
  NonPositive → npBounded s
{-# INLINE bounded' #-}

-- | A shorthand for 'bounded'' 'optMinus'.
bounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
           Monad μ, CharParsing μ) ⇒ s → μ α
bounded = bounded' optMinus
{-# INLINE bounded #-}

-- | Parse a number written in the specified positional numeral system,
--   failing on overflow. The supplied parser is used to determine the sign
--   of the number. Leading zeroes are not allowed.
cBounded' ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ)
          ⇒ μ Sign → s → μ α
cBounded' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBounded s
  NonPositive → npcBounded s
{-# INLINE cBounded' #-}

-- | A shorthand for 'cBounded'' 'optMinus'.
cBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
            Monad μ, CharParsing μ) ⇒ s → μ α
cBounded = cBounded' optMinus
{-# INLINE cBounded #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system. The supplied parser is used to determine
--   the sign of the number.
bits' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
      ⇒ μ Sign → s → μ α
bits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBits s
  NonPositive → npBits s
{-# INLINE bits' #-}

-- | A shorthand for 'bits'' 'optMinus'.
bits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
bits = bits' optMinus
{-# INLINE bits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system. The supplied parser is used to determine
--   the sign of the number. Leading zeroes are not allowed.
cBits' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
       ⇒ μ Sign → s → μ α
cBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBits s
  NonPositive → npcBits s
{-# INLINE cBits' #-}

-- | A shorthand for 'cBits'' 'optMinus'.
cBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
cBits = cBits' optMinus
{-# INLINE cBits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system (up to /n/ digits). The supplied parser is
--   used to determine the sign of the number.
bitsUpTo' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
          ⇒ μ Sign → s → Int → μ α
bitsUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBitsUpTo s n
  NonPositive → npBitsUpTo s n
{-# INLINE bitsUpTo' #-}

-- | A shorthand for 'bitsUpTo'' 'optMinus'.
bitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
         ⇒ s → Int → μ α
bitsUpTo = bitsUpTo' optMinus
{-# INLINE bitsUpTo #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system (up to /n/ digits). The supplied parser is
--   used to determine the sign of the number. Leading zeroes are not
--   allowed.
cBitsUpTo' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ μ Sign → s → Int → μ α
cBitsUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBitsUpTo s n
  NonPositive → npcBitsUpTo s n
{-# INLINE cBitsUpTo' #-}

-- | A shorthand for 'cBitsUpTo'' 'optMinus'.
cBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
          ⇒ s → Int → μ α
cBitsUpTo = cBitsUpTo' optMinus
{-# INLINE cBitsUpTo #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system, failing on overflow. The supplied parser is
--   used to determine the sign of the number.
bBits' ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
          Monad μ, CharParsing μ)
       ⇒ μ Sign → s → μ α
bBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnbBits s
  NonPositive → npbBits s
{-# INLINE bBits' #-}

-- | A shorthand for 'bBits'' 'optMinus'.
bBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
         Monad μ, CharParsing μ)
      ⇒ s → μ α
bBits = bBits' optMinus
{-# INLINE bBits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system, failing on overflow. The supplied parser is
--   used to determine the sign of the number. Leading zeroes are not
--   allowed.
cbBits' ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ μ Sign → s → μ α
cbBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncbBits s
  NonPositive → npcbBits s
{-# INLINE cbBits' #-}

-- | A shorthand for 'cbBits'' 'optMinus'.
cbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
          Monad μ, CharParsing μ)
       ⇒ s → μ α
cbBits = cbBits' optMinus
{-# INLINE cbBits #-}

