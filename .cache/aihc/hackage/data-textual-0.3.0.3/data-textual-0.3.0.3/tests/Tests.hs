{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>))

import Prelude hiding (print)
import Data.Word (Word)
import Data.Fixed (Pico)
import Type.Hint
import Control.Applicative
import Text.Printer (StringBuilder)
import qualified Text.Printer as TP
import qualified Text.Printer.Integral as TP
import qualified Text.Printer.Fractional as TP
import Text.Parser.Combinators as PC
import Text.Parser.Char (CharParsing)

import Data.Textual
import Data.Textual.Integral
import Data.Textual.Fractional

main = defaultMain
  [ testProperty "nonNegative Binary Int" $ \i →
      (i >= 0) ==>
        parse (nonNegative Binary) (TP.binary i) == Parsed (i ∷ Int)
  , testProperty "nonNegative Binary Word" $ \w →
      parse (nonNegative Binary) (TP.binary w) == Parsed (w ∷ Word)
  , testProperty "nonNegative Decimal Int" $ \i →
      (i >= 0) ==>
        parse (nonNegative Decimal) (TP.decimal i) == Parsed (i ∷ Int)
  , testProperty "nonNegative Decimal Word" $ \w →
      parse (nonNegative Decimal) (TP.decimal w) == Parsed (w ∷ Word)
  , testProperty "nonNegative Hexadecimal Int" $ \i →
      (i >= 0) ==>
        parse (nonNegative Hexadecimal) (TP.upHex i) == Parsed (i ∷ Int)
  , testProperty "nonNegative Hexadecimal Word" $ \w →
      parse (nonNegative Hexadecimal) (TP.lowHex w) == Parsed (w ∷ Word)
  , testProperty "nnCompact Decimal" $ \w →
      parse (nnCompact Decimal) (TP.nnDecimal w) == Parsed (w ∷ Word)
  , testProperty "nnCompact Decimal fails on \"00\"" $
      isMalformed $ parseAs aWord (nnCompact Decimal) "00"
  , testProperty "nnCompact Decimal fails on \"01\"" $
      isMalformed $ parseAs aWord (nnCompact Decimal) "01"
  , testProperty "nnUpTo Decimal 3" $ \w →
      case parse (nnUpTo Decimal 3) (TP.nnDecimal w) of
        Malformed _ _ | w >= 1000        → True
        Parsed w'     | w' == (w ∷ Word) → True 
        _                                → False
  , testProperty "nncUpTo Decimal 3" $ \w →
      case parse (nncUpTo Decimal 3) (TP.nnDecimal w) of
        Malformed _ _ | w >= 1000        → True
        Parsed w'     | w' == (w ∷ Word) → True 
        _                                → False
  , testProperty "nncUpTo Decimal fails on \"00\"" $
      isMalformed $ parseAs aWord (nncUpTo Decimal 3) "00"
  , testProperty "nncUpTo Decimal fails on \"01\"" $
      isMalformed $ parseAs aWord (nncUpTo Decimal 3) "01"
  , testProperty "nnBounded Hexadecimal Word" $ \w →
      parse (nnBounded Hexadecimal) (TP.nnUpHex w) == Parsed (w ∷ Word)
  , testProperty "nnBounded Hexadecimal Int" $ \i →
      (i >= 0) ==>
        parse (nnBounded Hexadecimal) (TP.nnUpHex i) == Parsed (i ∷ Int)
  , testProperty "nnBounded Hexadecimal Word fails on overflow" $
      isMalformed $ parseAs aWord (nnBounded Hexadecimal)
                            (TP.nnUpHex $ toInteger (maxBound ∷ Word) + 1)
  , testProperty "nnBounded Hexadecimal Int fails on overflow" $
      isMalformed $ parseAs anInt (nnBounded Hexadecimal)
                            (TP.nnUpHex $ toInteger (maxBound ∷ Int) + 1)
  , testProperty "nncBounded Decimal fails on \"00\"" $
      isMalformed $ parseAs aWord (nncBounded Decimal) "00"
  , testProperty "nncBounded Decimal fails on \"01\"" $
      isMalformed $ parseAs aWord (nncBounded Decimal) "01"
  , testProperty "nnBits Binary Int" $ \i →
      (i >= 0) ==>
        parse (nnBits Binary) (TP.binaryBits i) == Parsed (i ∷ Int)
  , testProperty "nnBits Binary Word" $ \w →
      parse (nnBits Binary) (TP.nnBinaryBits w) == Parsed (w ∷ Word)
  , testProperty "nnBits Hexadecimal Int" $ \i →
      (i >= 0) ==>
        parse (nnBits Hexadecimal) (TP.upHex i) == Parsed (i ∷ Int)
  , testProperty "nnBits Hexadecimal Word" $ \w →
      parse (nnBits Hexadecimal) (TP.nnLowHex w) == Parsed (w ∷ Word)
  , testProperty "nncBits Octal" $ \w →
      parse (nncBits Octal) (TP.nnOctal w) == Parsed (w ∷ Word)
  , testProperty "nncBits Octal fails on \"00\"" $
      isMalformed $ parseAs aWord (nncBits Octal) "00"
  , testProperty "nncBits Octal fails on \"01\"" $
      isMalformed $ parseAs aWord (nncBits Octal) "01"
  , testProperty "nnBitsUpTo Hexadecimal 3" $ \w →
      case parse (nnBitsUpTo Hexadecimal 3) (TP.nnLowHex w) of
        Malformed _ _ | w >= 0x1000      → True
        Parsed w'     | w' == (w ∷ Word) → True 
        _                                → False
  , testProperty "nncBitsUpTo Octal 3" $ \w →
      case parse (nncBitsUpTo Octal 3) (TP.nnOctal w) of
        Malformed _ _ | w >= 0o1000      → True
        Parsed w'     | w' == (w ∷ Word) → True 
        _                                → False
  , testProperty "nncBitsUpTo LowHex fails on \"00\"" $
      isMalformed $ parseAs aWord (nncBitsUpTo LowHex 3) "00"
  , testProperty "nncBitsUpTo LowHex fails on \"01\"" $
      isMalformed $ parseAs aWord (nncBitsUpTo LowHex 3) "01"
  , testProperty "nnbBits Hexadecimal Word" $ \w →
      parse (nnbBits Hexadecimal) (TP.nnUpHex w) == Parsed (w ∷ Word)
  , testProperty "nnbBits Hexadecimal Int" $ \i →
      (i >= 0) ==>
        parse (nnbBits Hexadecimal) (TP.nnLowHexBits i) == Parsed (i ∷ Int)
  , testProperty "nnbBits Hexadecimal Word fails on overflow" $
      isMalformed $ parseAs aWord (nnbBits Hexadecimal)
                            (TP.nnUpHexBits $ toInteger (maxBound ∷ Word) + 1)
  , testProperty "nnbBits Hexadecimal Int fails on overflow" $
      isMalformed $ parseAs anInt (nnbBits Hexadecimal)
                            (TP.nnUpHexBits $ toInteger (maxBound ∷ Int) + 1)
  , testProperty "nncbBits Octal fails on \"00\"" $
      isMalformed $ parseAs aWord (nncbBits Octal) "00"
  , testProperty "nncbBits Octal fails on \"01\"" $
      isMalformed $ parseAs aWord (nncbBits Octal) "01"
  , testProperty "nonPositive Binary" $ \i →
      (i <= 0) ==>
        parse (nonPositive Binary) (TP.npBinary i) == Parsed (i ∷ Int)
  , testProperty "nonPositive Decimal" $ \i →
      (i <= 0) ==>
        parse (nonPositive Decimal) (TP.npDecimal i) == Parsed (i ∷ Int)
  , testProperty "nonPositive Hexadecimal" $ \i →
      (i <= 0) ==>
        parse (nonPositive Hexadecimal) (TP.npLowHex i) == Parsed (i ∷ Int)
  , testProperty "npCompact Decimal" $ \i →
      (i <= 0) ==>
        parse (npCompact Decimal) (TP.npDecimal i) == Parsed (i ∷ Int)
  , testProperty "npCompact Decimal fails on \"00\"" $
      isMalformed $ parseAs anInt (npCompact Decimal) "00"
  , testProperty "npCompact Decimal fails on \"01\"" $
      isMalformed $ parseAs anInt (npCompact Decimal) "01"
  , testProperty "npUpTo Decimal 3" $ \i →
      (i <= 0) ==> case parse (npUpTo Decimal 3) (TP.npDecimal i) of
        Malformed _ _ | i <= -1000      → True
        Parsed i'     | i' == (i ∷ Int) → True 
        _                               → False
  , testProperty "npcUpTo Decimal 3" $ \i →
      (i <= 0) ==> case parse (npcUpTo Decimal 3) (TP.npDecimal i) of
        Malformed _ _ | i <= -1000      → True
        Parsed i'     | i' == (i ∷ Int) → True 
        _                               → False
  , testProperty "nncUpTo Decimal fails on \"00\"" $
      isMalformed $ parseAs aWord (nncUpTo Decimal 3) "00"
  , testProperty "nncUpTo Decimal fails on \"01\"" $
      isMalformed $ parseAs aWord (nncUpTo Decimal 3) "01"
  , testProperty "npBounded Hexadecimal" $ \i →
      (i <= 0) ==>
        parse (npBounded Hexadecimal) (TP.npLowHex i) == Parsed (i ∷ Int)
  , testProperty "npBounded Hexadecimal fails on overflow" $
      isMalformed $ parseAs anInt (npBounded Hexadecimal)
                            (TP.npLowHex $ toInteger (minBound ∷ Int) - 1)
  , testProperty "npcBounded Decimal fails on \"00\"" $
      isMalformed $ parseAs anInt (npcBounded Decimal) "00"
  , testProperty "npcBounded Decimal fails on \"01\"" $
      isMalformed $ parseAs anInt (npcBounded Decimal) "01"
  , testProperty "npBits Binary" $ \i →
      (i <= 0) ==>
        parse (npBits Binary) (TP.npBinaryBits i) == Parsed (i ∷ Int)
  , testProperty "npBits Hexadecimal" $ \i →
      (i <= 0) ==>
        parse (npBits Hexadecimal) (TP.npUpHex i) == Parsed (i ∷ Int)
  , testProperty "npcBits Octal" $ \i →
      (i <= 0) ==>
        parse (npcBits Octal) (TP.npOctal i) == Parsed (i ∷ Int)
  , testProperty "npcBits Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (npcBits Octal) "00"
  , testProperty "npcBits Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (npcBits Octal) "01"
  , testProperty "npBitsUpTo Hexadecimal 3" $ \i →
      (i <= 0) ==> case parse (npBitsUpTo Hexadecimal 3) (TP.npUpHex i) of
        Malformed _ _ | i <= -0x1000    → True
        Parsed i'     | i' == (i ∷ Int) → True 
        _                               → False
  , testProperty "npcBitsUpTo Octal 3" $ \i →
      (i <= 0) ==> case parse (npcBitsUpTo Octal 3) (TP.npOctal i) of
        Malformed _ _ | i <= -0o1000    → True
        Parsed i'     | i' == (i ∷ Int) → True 
        _                               → False
  , testProperty "npcBitsUpTo UpHex fails on \"00\"" $
      isMalformed $ parseAs anInt (npcBitsUpTo UpHex 3) "00"
  , testProperty "npcBitsUpTo UpHex fails on \"01\"" $
      isMalformed $ parseAs anInt (npcBitsUpTo UpHex 3) "01"
  , testProperty "npbBits Hexadecimal" $ \i →
      (i <= 0) ==>
        parse (npbBits Hexadecimal) (TP.npUpHexBits i) == Parsed (i ∷ Int)
  , testProperty "npbBits Hexadecimal fails on overflow" $
      isMalformed $ parseAs anInt (npbBits Hexadecimal)
                            (TP.npUpHexBits $ toInteger (minBound ∷ Int) - 1)
  , testProperty "npcbBits Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (npcbBits Octal) "00"
  , testProperty "npcbBits Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (npcbBits Octal) "01"
  , testProperty "number Decimal" $ \i →
      parse (number Decimal) (TP.decimal i) == Parsed (i ∷ Int)
  , testProperty "compact Decimal" $ \i →
      parse (number Decimal) (TP.decimal i) == Parsed (i ∷ Int)
  , testProperty "compact Decimal fails on \"00\"" $
      isMalformed $ parseAs anInt (compact Decimal) "00"
  , testProperty "compact Decimal fails on \"01\"" $
      isMalformed $ parseAs anInt (compact Decimal) "01"
  , testProperty "compact Decimal fails on \"-01\"" $
      isMalformed $ parseAs anInt (compact Decimal) "-01"
  , testProperty "numberUpTo Decimal 3" $ \i →
      case parseAs anInt (numberUpTo Decimal 3) (TP.decimal i) of
        Malformed _ _ | i >= 1000 || i <= -1000 → True
        Parsed i'     | i' == i                 → True 
        _                                       → False
  , testProperty "compactUpTo Decimal 3" $ \i →
      case parseAs anInt (compactUpTo Decimal 3) (TP.decimal i) of
        Malformed _ _ | i >= 1000 || i <= -1000 → True
        Parsed i'     | i' == i                 → True 
        _                                       → False
  , testProperty "compactUpTo Decimal fails on \"00\"" $
      isMalformed $ parseAs anInt (compactUpTo Decimal 3) "00"
  , testProperty "compactUpTo Decimal fails on \"01\"" $
      isMalformed $ parseAs anInt (compactUpTo Decimal 3) "01"
  , testProperty "compactUpTo Decimal fails on \"-01\"" $
      isMalformed $ parseAs anInt (compactUpTo Decimal 3) "-01"
  , testProperty "bounded Decimal" $ \i →
      parse (bounded Decimal) (TP.decimal i) == Parsed (i ∷ Int)
  , testProperty "bounded Decimal fails on overflow (vs maxBound)" $
      isMalformed $ parseAs anInt (bounded Decimal)
                            (TP.decimal $ toInteger (maxBound ∷ Int) + 1)
  , testProperty "bounded Decimal fails on overflow (vs minBound)" $
      isMalformed $ parseAs anInt (bounded Decimal)
                            (TP.decimal $ toInteger (minBound ∷ Int) - 1)
  , testProperty "cBounded Decimal" $ \i →
      parse (cBounded Decimal) (TP.decimal i) == Parsed (i ∷ Int)
  , testProperty "cBounded Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (cBounded Octal) "00"
  , testProperty "cBounded Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (cBounded Octal) "01"
  , testProperty "cBounded Octal fails on \"-01\"" $
      isMalformed $ parseAs anInt (cBounded Octal) "-01"
  , testProperty "bits Binary" $ \i →
      parse (bits Binary) (TP.binaryBits i) == Parsed (i ∷ Int)
  , testProperty "bits Hexadecimal" $ \i →
      parse (bits Hexadecimal) (TP.lowHexBits i) == Parsed (i ∷ Int)
  , testProperty "cBits Octal" $ \i →
      parse (cBits Octal) (TP.octalBits i) == Parsed (i ∷ Int)
  , testProperty "cBits Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (cBits Octal) "00"
  , testProperty "cBits Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (cBits Octal) "01"
  , testProperty "cBits Octal fails on \"-01\"" $
      isMalformed $ parseAs anInt (cBits Octal) "-01"
  , testProperty "bitsUpTo LowHex 3" $ \i →
      case parseAs anInt (bitsUpTo LowHex 3) (TP.lowHex i) of
        Malformed _ _ | i >= 0x1000 || i <= -0x1000 → True
        Parsed i'     | i' == i                     → True 
        _                                           → False
  , testProperty "cBitsUpTo UpHex 3" $ \i →
      case parseAs anInt (cBitsUpTo UpHex 3) (TP.upHex i) of
        Malformed _ _ | i >= 0x1000 || i <= -0x1000 → True
        Parsed i'     | i' == i                     → True 
        _                                           → False
  , testProperty "cBitsUpTo Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (cBitsUpTo Octal 3) "00"
  , testProperty "cBitsUpTo Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (cBitsUpTo Octal 3) "01"
  , testProperty "cBitsUpTo Octal fails on \"-01\"" $
      isMalformed $ parseAs anInt (cBitsUpTo Octal 3) "-01"
  , testProperty "bBits LowHex" $ \i →
      parse (bBits LowHex) (TP.lowHexBits i) == Parsed (i ∷ Int)
  , testProperty "bBits LowHex fails on overflow (vs maxBound)" $
      isMalformed $ parseAs anInt (bBits LowHex)
                            (TP.lowHexBits $ toInteger (maxBound ∷ Int) + 1)
  , testProperty "bBits UpHex fails on overflow (vs minBound)" $
      isMalformed $ parseAs anInt (bBits UpHex)
                            (TP.upHexBits $ toInteger (minBound ∷ Int) - 1)
  , testProperty "cbBits Octal" $ \i →
      parse (cbBits Octal) (TP.octal i) == Parsed (i ∷ Int)
  , testProperty "cbBits Octal fails on \"00\"" $
      isMalformed $ parseAs anInt (cbBits Octal) "00"
  , testProperty "cbBits Octal fails on \"01\"" $
      isMalformed $ parseAs anInt (cbBits Octal) "01"
  , testProperty "cbBits Octal fails on \"-01\"" $
      isMalformed $ parseAs anInt (cbBits Octal) "-01"
  , testProperty "fraction" $ \i → 
      parse fraction (TP.fraction i) == Parsed (i ∷ Rational)
  , testProperty "fractional (Pico)" $ \i → 
      parse fractional (print i) == Parsed (i ∷ Pico)
  , testProperty "fractional (Float)" $ \i → 
      (not (isInfinite i) && not (isNaN i)) ==>
        parse fractional (print i) == Parsed (i ∷ Float)
  , testProperty "fractional (Double)" $ \i → 
      (not (isInfinite i) && not (isNaN i)) ==>
        parse fractional (print i) == Parsed (i ∷ Double)
  ]

parse ∷ (∀ μ . (Monad μ, CharParsing μ) ⇒ μ α) → StringBuilder → Parsed α
parse p b = builtInParser (p <* PC.eof) (TP.buildString b)

parseAs ∷ p α → (∀ μ . (Monad μ, CharParsing μ) ⇒ μ α) → StringBuilder
        → Parsed α
parseAs _ = parse

