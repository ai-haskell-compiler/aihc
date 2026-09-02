module Data.Char
  ( Char,
    GeneralCategory (..),
    chr,
    digitToInt,
    intToDigit,
    generalCategory,
    isAlpha,
    isAlphaNum,
    isAscii,
    isAsciiLower,
    isAsciiUpper,
    isControl,
    isDigit,
    isHexDigit,
    isLatin1,
    isLetter,
    isLower,
    isLowerCase,
    isMark,
    isNumber,
    isOctDigit,
    isPrint,
    isPunctuation,
    isSeparator,
    isSpace,
    isSymbol,
    isUpper,
    isUpperCase,
    ord,
    toLower,
    toTitle,
    toUpper,
  )
where

import GHC.Char (chr, ord)
import GHC.Show (intToDigit)
import GHC.Unicode
import Prelude (Bool (..), Char, Int, Num (..), Ord (..), errorWithoutStackTrace, (&&))

digitToInt :: Char -> Int
digitToInt character = digitValue (ord character)

digitValue :: Int -> Int
digitValue codePoint =
  case (48 <= codePoint && codePoint <= 57, 97 <= codePoint && codePoint <= 102, 65 <= codePoint && codePoint <= 70) of
    (True, _, _) -> codePoint - 48
    (_, True, _) -> codePoint - 87
    (_, _, True) -> codePoint - 55
    _ -> errorWithoutStackTrace "Char.digitToInt: not a digit"
