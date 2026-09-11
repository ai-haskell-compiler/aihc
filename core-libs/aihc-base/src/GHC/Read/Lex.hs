-- | The lexer of the 'Read' class.
--
-- The lexer itself lives in "GHC.Prim.Read", so that a derived 'Read'
-- instance needs the primitive package only.
module GHC.Read.Lex
  ( Lexeme (..),
    NumberToken (..),
    lex,
    lexDigits,
    lexLitChar,
    readLitChar,
    lexP,
    expectP,
    parseSignedInteger,
    stringEqual,
    numberToInteger,
    numberToFixed,
    numberToRational,
  )
where

import GHC.Prim.Read
  ( Lexeme (..),
    NumberToken (..),
    expectP,
    lexDigits,
    lexLitChar,
    lexP,
    parseSignedInteger,
    readLitChar,
    stringEqual,
  )
import GHC.Real ((%))
import Prelude

-- | The value of an integer literal; a literal with a fraction or an
-- exponent has none.
numberToInteger :: NumberToken -> Maybe Integer
numberToInteger (NumberToken value) = Just value
numberToInteger DecimalToken {} = Nothing

-- | The whole part and the first @digits@ fraction digits of a literal, as
-- a fixed-point reader needs them. A literal with an exponent is refused.
numberToFixed :: Integer -> NumberToken -> Maybe (Integer, Integer)
numberToFixed _ (NumberToken value) = Just (value, 0)
numberToFixed digits (DecimalToken whole fraction 0) =
  Just (whole, digitsToInteger (take (fromInteger digits) (fraction ++ repeat '0')))
numberToFixed _ _ = Nothing

-- | The exact value of a literal.
numberToRational :: NumberToken -> Rational
numberToRational (NumberToken value) = fromInteger value
numberToRational (DecimalToken whole fraction exponent) =
  let mantissa = whole * 10 ^ length fraction + digitsToInteger fraction
      scaled = mantissa % 10 ^ length fraction
   in case exponent >= 0 of
        True -> scaled * fromInteger (10 ^ exponent)
        False -> scaled / fromInteger (10 ^ negate exponent)

digitsToInteger :: String -> Integer
digitsToInteger = foldl (\value digit -> value * 10 + toInteger (fromEnum digit - fromEnum '0')) 0
