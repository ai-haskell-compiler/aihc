module Numeric
  ( showSigned,
    showInt,
    showIntAtBase,
    showHex,
    showOct,
    showBin,
    readInt,
    readBin,
    readOct,
    readDec,
    readHex,
    readSigned,
    readFloat,
    lexDigits,
    fromRat,
    floatToDigits,
    showEFloat,
    showFFloat,
    showGFloat,
    showFloat,
    showHFloat,
    Floating (..),
  )
where

import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit, isSpace)
import GHC.Float (FFFormat (..), Floating (..), floatToDigits, formatRealFloat, showFloat)
import GHC.Read.Lex (Lexeme (..), lexP, numberToRational)
import GHC.Show (intToDigit)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Prelude (Bool (..), Char, Eq (..), Fractional (..), Int, Integral (..), Maybe, Num (..), Ord (..), Rational, ReadS, Real (..), RealFloat (..), RealFrac, Show (..), ShowS, String, dropWhile, errorWithoutStackTrace, fromIntegral, id, negate, otherwise, readParen, showChar, showParen, showString, span, (&&), (++), (.), (||))

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPositive precedence value =
  if value < 0
    then showParen (precedence > 6) (showChar '-' . showPositive (negate value))
    else showPositive value

showInt :: (Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toDigit value rest =
  case (base <= 1, value < 0) of
    (True, _) -> errorWithoutStackTrace "Numeric.showIntAtBase: unsupported base"
    (_, True) -> errorWithoutStackTrace "Numeric.showIntAtBase: negative number"
    _ -> showDigits base toDigit value rest

showDigits :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showDigits base toDigit value rest =
  case quotRem value base of
    (quotient, remainder) ->
      let digits = toDigit (fromIntegral remainder) : rest
       in if quotient == 0
            then digits
            else showDigits base toDigit quotient digits

showHex :: (Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: (Integral a) => a -> ShowS
showOct = showIntAtBase 8 intToDigit

showBin :: (Integral a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

-- | Read an unsigned number written in a positional base.
--
-- @isDigitChar@ accepts a digit of the base and @digitValue@ gives its
-- value. The reader fails when the input does not start with a digit.
readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt base isDigitChar digitValue input =
  case span isDigitChar input of
    ([], _) -> []
    (digits, rest) -> [(accumulate 0 digits, rest)]
  where
    accumulate accumulator remaining =
      case remaining of
        [] -> accumulator
        digit : more -> accumulate (accumulator * base + fromIntegral (digitValue digit)) more

-- | Read an unsigned binary number.
readBin :: (Num a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt

-- | Read an unsigned octal number.
readOct :: (Num a) => ReadS a
readOct = readInt 8 isOctDigit digitToInt

-- | Read an unsigned decimal number.
readDec :: (Num a) => ReadS a
readDec = readInt 10 isDigit digitToInt

-- | Read an unsigned hexadecimal number. Both letter cases are accepted.
readHex :: (Num a) => ReadS a
readHex = readInt 16 isHexDigit digitToInt

-- | Add an optional sign and optional parentheses to an unsigned reader.
--
-- The reader skips the space before the sign and before the number.
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPositive = readParen False readSigned'
  where
    readSigned' input =
      case dropWhile isSpace input of
        '-' : afterSign -> [(negate value, rest) | (value, rest) <- readPositive (dropWhile isSpace afterSign)]
        withoutSpace -> readPositive withoutSpace

-- | Read the longest non-empty run of decimal digits.
lexDigits :: ReadS String
lexDigits input =
  case span isDigit input of
    ([], _) -> []
    (digits, rest) -> [(digits, rest)]

isBinDigit :: Char -> Bool
isBinDigit character = character == '0' || character == '1'

-- | Show a number in scientific notation, with the given number of digits
-- after the decimal point, or every significant digit for @Nothing@.
showEFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat decimals value = showString (formatRealFloat FFExponent decimals value)

-- | Show a number in standard decimal notation, with the given number of
-- digits after the decimal point, or every significant digit for @Nothing@.
showFFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat decimals value = showString (formatRealFloat FFFixed decimals value)

-- | Show a number in standard decimal notation for 0.1 <= x < 10^7, and in
-- scientific notation for other numbers.
showGFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat decimals value = showString (formatRealFloat FFGeneric decimals value)

-- | Read an unsigned decimal number. The number can have a fraction and an
-- exponent. The reader skips the space before the number.
readFloat :: (RealFrac a) => ReadS a
readFloat input =
  [(fromRational (numberToRational token), rest) | (Number token, rest) <- readPrec_to_S lexP 0 input]

-- | Convert a rational number to a floating-point number.
fromRat :: (RealFloat a) => Rational -> a
fromRat = fromRational

-- | Show a floating-point number in hexadecimal notation, as in C99.
-- For example, @showHFloat (212.21 :: Double) ""@ gives
-- @"0x1.a86b851eb851fp7"@.
showHFloat :: (RealFloat a) => a -> ShowS
showHFloat value = showString (hexFloat value)

hexFloat :: (RealFloat a) => a -> String
hexFloat value
  | isNaN value = "NaN"
  | isInfinite value = (if value < 0 then "-" else "") ++ "Infinity"
  | value < 0 || isNegativeZero value = '-' : hexMagnitude (negate value)
  | otherwise = hexMagnitude value

hexMagnitude :: (RealFloat a) => a -> String
hexMagnitude value
  | value == 0 = "0x0p+0"
  | otherwise =
      case floatToDigits 2 value of
        (leading : bits, exponent') -> "0x" ++ show leading ++ hexFraction bits ++ "p" ++ show (exponent' - 1)
        ([], _) -> errorWithoutStackTrace "Numeric.showHFloat: no digits"

-- | Group the binary digits of the fraction into hexadecimal digits. A
-- fraction with only zero digits shows as nothing.
hexFraction :: [Int] -> String
hexFraction bits
  | allZero bits = ""
  | otherwise = '.' : hexDigits bits ""
  where
    allZero remaining =
      case remaining of
        [] -> True
        bit : more -> bit == 0 && allZero more

hexDigits :: [Int] -> ShowS
hexDigits bits =
  case bits of
    [] -> id
    [a] -> hexDigit a 0 0 0
    [a, b] -> hexDigit a b 0 0
    [a, b, c] -> hexDigit a b c 0
    a : b : c : d : more -> hexDigit a b c d . hexDigits more
  where
    hexDigit a b c d = showHex (8 * a + 4 * b + 2 * c + d)
