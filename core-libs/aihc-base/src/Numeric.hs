module Numeric
  ( showSigned,
    showInt,
    showIntAtBase,
    showHex,
    showOct,
    showBin,
  )
where

import GHC.Show (intToDigit)
import Prelude (Bool (..), Char, Eq (..), Int, Integral (..), Num (..), Ord (..), Real (..), Show (..), ShowS, errorWithoutStackTrace, fromIntegral, showChar, showParen, (.))

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
