{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | Parsers for fractions.
module Data.Textual.Fractional
  (
  -- * Positional numeral systems
    PositionalSystem(..)
  , Binary(..)
  , Octal(..)
  , Decimal(..)
  , Hexadecimal(..)
  , UpHex(..)
  , LowHex(..)
  -- * Sign
  , Sign(..)
  , applySign
  , optMinus
  , optSign
  -- * Optionality characteristic
  , Optional(..)
  , isOptional
  , isRequired
  -- * Fraction parsers
  , optSlash
  , fraction'
  , fraction
  -- * s-fraction parsers
  , decExpSign
  , hexExpSign
  , fractional'
  , fractional
  ) where

import Data.Maybe (isJust)
import Data.Ratio ((%))
import Control.Applicative
import Text.Printer.Fractional (Optional(..), isOptional, isRequired)
import Text.Parser.Combinators ((<?>), unexpected)
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC
import Data.Textual.Integral

-- | Accept a slash and return 'Required'. Otherwise return 'Optional'.
optSlash ∷ (Monad μ, CharParsing μ) ⇒ μ Optional
optSlash = maybe Optional (const Required) <$> optional (PC.char '/')

-- | Parse a fraction. The numerator and the denominator are expected to be
--   written in the specified positional numeral system.
fraction' ∷ (PositionalSystem s, Fractional α, Monad μ, CharParsing μ)
          ⇒ μ Sign -- ^ Sign parser
          → s
          → μ Optional -- ^ Numerator/denominator separator parser
          → μ α
fraction' neg s den = (<?> "fraction") $ do
  n ← number' neg s <?> "numerator"
  den >>= \case
    Optional →
      return $ fromInteger n
    Required → do
      d ← (<?> "denominator") $ do
        d ← nonNegative s
        if d == 0 then unexpected "zero denominator"
                  else return d
      return $ fromRational $ n % d

-- | A shorthand for 'fraction'' 'optMinus' 'Decimal' 'optSlash'.
fraction ∷ (Fractional α, Monad μ, CharParsing μ) ⇒ μ α
fraction = fraction' optMinus Decimal optSlash

-- | Start of a decimal exponent. Accepts /'e'/ or /'E'/ followed by
--   an optional sign. Otherwise 'Nothing' is returned.
decExpSign ∷ (Monad μ, CharParsing μ) ⇒ μ (Maybe Sign)
decExpSign = optional (PC.oneOf "eE") >>= \case
               Nothing → return Nothing
               Just _  → Just <$> optSign

-- | Start of a hexadecimal exponent. Accepts /'p'/ or /'P'/ followed by
--   an optional sign. Otherwise 'Nothing' is returned.
hexExpSign ∷ (Monad μ, CharParsing μ) ⇒ μ (Maybe Sign)
hexExpSign = optional (PC.oneOf "pP") >>= \case
               Nothing → return Nothing
               Just _  → Just <$> optSign

-- | /s/-fraction parser.
fractional' ∷ (PositionalSystem s, Fractional α, Monad μ, CharParsing μ)
            ⇒ μ Sign -- ^ Sign parser.
            → s
            → Optional -- ^ Whether the integer part is optional.
            → μ () -- ^ Dot parser.
            → μ (Maybe Sign) -- ^ Exponent start parser.
            → μ α
fractional' neg s ip dot eneg = (<?> (systemName s ++ "-fraction")) $ do
    sign ← neg <?> "sign"
    (i, f, fDigits) ← do
      let integral = do
            i ← nonNegative s <?> "integer part"
            ((i, ) . isJust) <$> optional dot
      (i, hasF) ← case ip of
        Optional → optional dot >>= \case
          Nothing → integral
          Just _ → return (0, True)
        Required → integral
      (f, fDigits) ←
        if hasF
        then do
          let go !ds !f = optional digit >>= \case
                            Just d  → go (ds + 1) (f * radix + d)
                            Nothing → return (f, ds) 
          digit >>= go (1 ∷ Int) <?> "fractional part"
        else
          return (0, 0)
      return (i, f, fDigits)
    (<?> "exponent") $ eneg >>= \case
      Nothing | f == 0    → return $ fromInteger $ applySign sign i
              | otherwise → return $ fromRational
                                   $ applySign sign
                                   $ fromInteger i + f % radix ^ fDigits
      Just esign → do
        e ← nnBounded Decimal
        return $ applySign sign $ case esign of
          NonNegative → case e - fDigits of
            e₁ | e₁ >= 0   → fromInteger $ i * radix ^ e + f * radix ^ e₁
               | otherwise → fromRational
                           $ fromInteger (i * radix ^ e)
                           + i % radix ^ negate e₁
          NonPositive → fromRational
                      $ i % (radix ^ e) + f % radix ^ (fDigits + e)
  where 
    radix = radixIn s
    digit = digitIn s

-- | Decimal fraction parser.
fractional ∷ (Monad μ, Fractional α, CharParsing μ) ⇒ μ α
fractional = fractional' optMinus Decimal Required
                         (PC.char '.' *> pure ()) decExpSign
