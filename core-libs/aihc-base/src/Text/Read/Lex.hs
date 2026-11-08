module Text.Read.Lex
  ( Lexeme (..),
    expect,
    hsLex,
    numberToInteger,
    numberToFixed,
    numberToRational,
    readIntP,
    readBinP,
    readOctP,
    readDecP,
    readHexP,
  )
where

import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit)
import GHC.Read.Lex (Lexeme (..), expectP, lexP, numberToFixed, numberToInteger, numberToRational)
import Text.ParserCombinators.ReadP (ReadP, gather, munch1, skipSpaces)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_P)
import Prelude

-- | Skip white space and read the given lexeme. Fail on a different lexeme.
expect :: Lexeme -> ReadP ()
expect lexeme = readPrec_to_P (expectP lexeme) minPrec

-- | Skip white space and read one Haskell lexeme. Give the text of the lexeme.
hsLex :: ReadP String
hsLex = do
  skipSpaces
  (text, _) <- gather (readPrec_to_P lexP minPrec)
  return text

-- | Read one or more digits in the given base.
-- The predicate selects the digits. The function gives the value of a digit.
readIntP :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP base isDigitInBase digitValue = do
  digits <- munch1 isDigitInBase
  return (foldl (\acc digit -> acc * base + fromIntegral (digitValue digit)) 0 digits)

-- | Read one or more binary digits.
readBinP :: (Eq a, Num a) => ReadP a
readBinP = readIntP 2 (\c -> c == '0' || c == '1') digitToInt

-- | Read one or more octal digits.
readOctP :: (Eq a, Num a) => ReadP a
readOctP = readIntP 8 isOctDigit digitToInt

-- | Read one or more decimal digits.
readDecP :: (Eq a, Num a) => ReadP a
readDecP = readIntP 10 isDigit digitToInt

-- | Read one or more hexadecimal digits.
readHexP :: (Eq a, Num a) => ReadP a
readHexP = readIntP 16 isHexDigit digitToInt
