{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>))

import Data.Word (Word)
import Data.Char (intToDigit)
import Data.Ratio (numerator, denominator)
import Numeric (showIntAtBase)
import Text.Printf (printf)
import Text.Printer.Integral
import Text.Printer.Fractional

main = defaultMain
  [ testProperty "nnBinary" $ \w →
      nnBinary w == showBinary (w ∷ Word)
  , testProperty "nnBinaryBits" $ \w →
      nnBinaryBits w == showBinary (w ∷ Word)
  , testProperty "npBinary" $ \i →
      i <= 0 ==> npBinary (i ∷ Int) == showBinary (negate $ toInteger i)
  , testProperty "npBinaryBits" $ \i →
      i <= 0 ==> npBinaryBits (i ∷ Int) == showBinary (negate $ toInteger i)
  , testProperty "nnOctal" $ \w →
      nnOctal w == (printf "%o" (w ∷ Word) ∷ String)
  , testProperty "nnOctalBits" $ \w →
      nnOctalBits w == (printf "%o" (w ∷ Word) ∷ String)
  , testProperty "npOctal" $ \i →
      i <= 0 ==> npOctal (i ∷ Int) ==
                   (printf "%o" (negate $ toInteger i) ∷ String)
  , testProperty "npOctalBits" $ \i →
      i <= 0 ==> npOctalBits (i ∷ Int) ==
                   (printf "%o" (negate $ toInteger i) ∷ String)
  , testProperty "nnDecimal" $ \w →
      nnDecimal w == show (w ∷ Word)
  , testProperty "npDecimal" $ \i →
      i <= 0 ==> npDecimal (i ∷ Int) == show (negate $ toInteger i)
  , testProperty "nnLowHex" $ \w →
      nnLowHex w == (printf "%x" (w ∷ Word) ∷ String)
  , testProperty "nnLowHexBits" $ \w →
      nnLowHexBits w == (printf "%x" (w ∷ Word) ∷ String)
  , testProperty "npLowHex" $ \i →
      i <= 0 ==> npLowHex (i ∷ Int) ==
                   (printf "%x" (negate $ toInteger i) ∷ String)
  , testProperty "npLowHexBits" $ \i →
      i <= 0 ==> npLowHexBits (i ∷ Int) ==
                   (printf "%x" (negate $ toInteger i) ∷ String)
  , testProperty "nnUpHex" $ \w →
      nnUpHex w == (printf "%X" (w ∷ Word) ∷ String)
  , testProperty "nnUpHexBits" $ \w →
      nnUpHexBits w == (printf "%X" (w ∷ Word) ∷ String)
  , testProperty "npUpHex" $ \i →
      i <= 0 ==> npUpHex (i ∷ Int) ==
                   (printf "%X" (negate $ toInteger i) ∷ String)
  , testProperty "npLowHexBits" $ \i →
      i <= 0 ==> npUpHexBits (i ∷ Int) ==
                   (printf "%X" (negate $ toInteger i) ∷ String)
  , testProperty "binary" $ \i →
      if (i ∷ Int) == 0
      then binary i == "0"
      else if i < 0
           then binary i == '-' : showBinary (negate $ toInteger i)
           else binary i == showBinary i
  , testProperty "binaryBits" $ \i →
      if (i ∷ Int) == 0
      then binaryBits i == "0"
      else if i < 0
           then binaryBits i == '-' : showBinary (negate $ toInteger i)
           else binaryBits i == showBinary i
  , testProperty "octal" $ \i →
      if (i ∷ Int) == 0
      then octal i == "0"
      else if i < 0
           then octal i == (printf "-%o" (negate $ toInteger i) ∷ String)
           else octal i == (printf "%o" i ∷ String)
  , testProperty "octalBits" $ \i →
      if (i ∷ Int) == 0
      then octalBits i == "0"
      else if i < 0
           then octalBits i ==
                  (printf "-%o" (negate $ toInteger i) ∷ String)
           else octalBits i == (printf "%o" i ∷ String)
  , testProperty "decimal" $ \i →
      decimal i == show (i ∷ Int)
  , testProperty "lowHex" $ \i →
      if (i ∷ Int) == 0
      then lowHex i == "0"
      else if i < 0
           then lowHex i == (printf "-%x" (negate $ toInteger i) ∷ String)
           else lowHex i == (printf "%x" i ∷ String)
  , testProperty "lowHexBits" $ \i →
      if (i ∷ Int) == 0
      then lowHexBits i == "0"
      else if i < 0
           then lowHexBits i == (printf "-%x" (negate $ toInteger i) ∷ String)
           else lowHexBits i == (printf "%x" i ∷ String)
  , testProperty "upHex" $ \i →
      if (i ∷ Int) == 0
      then upHex i == "0"
      else if i < 0
           then upHex i == (printf "-%X" (negate $ toInteger i) ∷ String)
           else upHex i == (printf "%X" i ∷ String)
  , testProperty "upHexBits" $ \i →
      if (i ∷ Int) == 0
      then upHexBits i == "0"
      else if i < 0
           then upHexBits i == (printf "-%X" (negate $ toInteger i) ∷ String)
           else upHexBits i == (printf "%X" i ∷ String)
  , testProperty "fraction" $ \r →
      if r == (0 ∷ Rational)
      then fraction r == "0"
      else let n = numerator r
               d = denominator r in
             fraction r == if d == 1 then show n
                                     else show n ++ "/" ++ show d
  ]

showBinary i = showIntAtBase 2 intToDigit i ""

