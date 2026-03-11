{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric #-}
#endif

-- | Print fractions.
module Text.Printer.Fractional
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
  -- * Optionality characteristic
  , Optional(..)
  , isOptional
  , isRequired
  -- * Fraction printers
  , fraction'
  , fraction
  ) where

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic)
#endif
import Data.Typeable (Typeable)
import Data.Ix (Ix)
import Data.Monoid (mempty)
import Data.Ratio (numerator, denominator)
import Text.Printer
import Text.Printer.Integral

-- | Optionality characteristic.
data Optional = Optional
              | Required
              deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                       , Generic
#endif
                       , Show, Read, Eq, Ord, Enum, Bounded, Ix)

-- | True if the supplied value is 'Optional' and false otherwise.
isOptional ∷ Optional → Bool
isOptional Optional = True
isOptional Required = False

-- | True if the supplied value is 'Required' and false otherwise.
isRequired ∷ Optional → Bool
isRequired Optional = False
isRequired Required = True

-- | Print a fraction, writing the numerator and the denominator in
--   the specified positional numeral system.
fraction' ∷ (PositionalSystem s, Real α, Printer p)
          ⇒ s
          → p -- ^ Prefix for negative values
          → p -- ^ Zero printer
          → p -- ^ Prefix for positive values
          → p -- ^ Numerator/denominator separator
          → Optional -- ^ Whether to print invisible denominators
          → α → p
fraction' s neg z pos sep i a
    | n == 0    = z
    | d == 1    = case i of
                    Optional → number' s neg z pos n
                    Required →  number' s neg z pos n
                             <> sep
                             <> (printDigitIn s $! intToDigitIn s 1)
    | otherwise = if n < 0
                  then neg <> nonPositive s n <> sep <> nonNegative s d
                  else pos <> nonNegative s n <> sep <> nonNegative s d
  where r = toRational a
        n = numerator r
        d = denominator r

-- | Print a fraction. The numerator and the denominator are written in the
--   decimal numeral system and separated by a slash. Negative values
--   are prefixed with a minus sign. Invisible denominators are omitted.
fraction ∷ (Real α, Printer p) ⇒ α → p
fraction = fraction' Decimal (char7 '-') (char7 '0')
                     mempty (char7 '/') Optional
