{-# LANGUAGE StandaloneDeriving #-}

-- | The lexer of the 'Read' class.
--
-- The lexer itself lives in "GHC.Prim.Read", so that a derived 'Read'
-- instance needs the primitive package only.
module GHC.Read.Lex
  ( Lexeme (..),
    Number (..),
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
    numberToRangedRational,
  )
where

import GHC.Internal.Read (numberToRational)
import GHC.Prim.Read
  ( Lexeme (..),
    Number (..),
    expectP,
    lexDigits,
    lexLitChar,
    lexP,
    numberDigitsValue,
    parseSignedInteger,
    readLitChar,
    stringEqual,
  )
import Prelude

deriving stock instance Eq Number

deriving stock instance Show Number

deriving stock instance Eq Lexeme

deriving stock instance Show Lexeme

-- | The value of an integer literal, as in GHC. A literal with a fraction or
-- an exponent has none.
numberToInteger :: Number -> Maybe Integer
numberToInteger (MkNumber base iPart) = Just (numberDigitsValue (toInteger base) iPart)
numberToInteger (MkDecimal iPart Nothing Nothing) = Just (numberDigitsValue 10 iPart)
numberToInteger _ = Nothing

-- | The whole part and the first fraction digits of a literal, as a
-- fixed-point reader needs them, as in GHC. A literal with an exponent has
-- none.
numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed _ (MkNumber base iPart) = Just (numberDigitsValue (toInteger base) iPart, 0)
numberToFixed _ (MkDecimal iPart Nothing Nothing) = Just (numberDigitsValue 10 iPart, 0)
numberToFixed p (MkDecimal iPart (Just fPart) Nothing) =
  let i = numberDigitsValue 10 iPart
      f = numberDigitsValue 10 (integerTake p (fPart ++ repeat 0))
   in Just (i, f)
numberToFixed _ _ = Nothing

integerTake :: Integer -> [a] -> [a]
integerTake n _ | n <= 0 = []
integerTake _ [] = []
integerTake n (x : xs) = x : integerTake (n - 1) xs

-- | The value of a literal, or 'Nothing' if the literal is too large for the
-- range of exponents, as in GHC. A literal that is too small gives zero.
numberToRangedRational :: (Int, Int) -> Number -> Maybe Rational
numberToRangedRational (neg, pos) n@(MkDecimal iPart mFPart (Just exponentValue)) =
  let mFirstDigit = case dropWhile (0 ==) iPart of
        iPart'@(_ : _) -> Just (length iPart')
        [] -> case mFPart of
          Nothing -> Nothing
          Just fPart ->
            case span (0 ==) fPart of
              (_, []) -> Nothing
              (zeroes, _) -> Just (negate (length zeroes))
   in case mFirstDigit of
        Nothing -> Just 0
        Just firstDigit ->
          let firstDigit' = toInteger firstDigit + exponentValue
           in if firstDigit' > toInteger (pos + 3)
                then Nothing
                else
                  if firstDigit' < toInteger (neg - 3)
                    then Just 0
                    else Just (numberToRational n)
numberToRangedRational _ n = Just (numberToRational n)
