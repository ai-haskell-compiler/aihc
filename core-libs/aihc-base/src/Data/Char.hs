module Data.Char
  ( Char,
    GeneralCategory (..),
    chr,
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
import GHC.Unicode
import Prelude (Char)
