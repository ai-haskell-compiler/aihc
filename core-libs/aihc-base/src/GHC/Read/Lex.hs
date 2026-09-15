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

import GHC.Internal.Read (numberToRational)
import GHC.Prim.Read
  ( Lexeme (..),
    NumberToken (..),
    digitsToInteger,
    expectP,
    lexDigits,
    lexLitChar,
    lexP,
    parseSignedInteger,
    readLitChar,
    stringEqual,
  )
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
  Just (whole, digitsToInteger 10 (take (fromInteger digits) (fraction ++ repeat '0')))
numberToFixed _ _ = Nothing
