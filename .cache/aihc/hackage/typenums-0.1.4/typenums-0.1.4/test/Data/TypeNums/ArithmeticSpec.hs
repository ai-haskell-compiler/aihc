{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

module Data.TypeNums.ArithmeticSpec where

import Data.TypeNums
import Test.Hspec(Spec, describe, it, shouldBe)

import TestUtil(typesShouldBeEqual)

spec :: Spec
spec = do
  additionTests
  subtractionTests
  multiplicationTests
  absTests
  negateTests
  recipTests
  divisionTests
  divModTests
  quotRemTests
  gcdTests
  simplifyTests
  expTests
  roundingTests
  logTests

additionTests :: Spec
additionTests =
  describe "addition" $ do
    it "correctly adds two natural numbers" $
      typesShouldBeEqual @Nat @(3 + 5) @8
    it "correctly adds a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Neg 4 + 3) @(Neg 1)
    it "correctly adds a natural and a negative integer" $
      typesShouldBeEqual @TInt @(8 + Neg 7) @(Pos 1)
    it "correctly adds two negative integers" $
      typesShouldBeEqual @TInt @(Neg 5 + Neg 6) @(Neg 11)
    it "correctly adds two positive integers" $
      typesShouldBeEqual @TInt @(Pos 5 + Pos 6) @(Pos 11)
    it "correctly adds a negative integer and a positive integer" $
      typesShouldBeEqual @TInt @(Neg 3 + Pos 5) @(Pos 2)
    it "correctly adds a positive integer and a negative integer" $
      typesShouldBeEqual @TInt @(Pos 7 + Neg 8) @(Neg 1)
    it "correctly adds two rationals" $
      typesShouldBeEqual @Rat @(2:%3 + 5:%7) @(Pos 29 :% 21)
    it "correctly adds a rational with a natural" $
      typesShouldBeEqual @Rat @(2:%3 + 2) @(Pos 8 :% 3)
    it "correctly adds a natural with a rational" $
      typesShouldBeEqual @Rat @(2 + 2:%3) @(Pos 8 :% 3)
    it "correctly adds a rational with a negative integer" $
      typesShouldBeEqual @Rat @(2':%3 + Neg 2) @(Neg 4 :% 3)
    it "correctly adds a negative integer with a rational" $
      typesShouldBeEqual @Rat @(Neg 2 + 2:%3) @(Neg 4 :% 3)
    it "correctly adds a rational with a positive integer" $
      typesShouldBeEqual @Rat @(2:%3 + Pos 2) @(Pos 8 :% 3)
    it "correctly adds a positive integer with a rational" $
      typesShouldBeEqual @Rat @(Pos 2 + 2:%3) @(Pos 8 :% 3)
    it "correctly simplifies the result of adding two rationals" $
      typesShouldBeEqual @Rat @(3:%4 + 3:%4) @(Pos 3 :% 2)

subtractionTests :: Spec
subtractionTests =
  describe "subtraction" $ do
    it "correctly subtracts two natural numbers" $
      typesShouldBeEqual @Nat @(5-3) @2
    it "correctly subtracts a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Neg 4 - 3) @(Neg 7)
    it "correctly subtracts a natural and a negative integer" $
      typesShouldBeEqual @TInt @(8 - Neg 7) @(Pos 15)
    it "correctly subtracts two negative integers" $
      typesShouldBeEqual @TInt @(Neg 5 - Neg 6) @(Pos 1)
    it "correctly subtracts two positive integers" $
      typesShouldBeEqual @TInt @(Pos 5 - Pos 6) @(Neg 1)
    it "correctly subtracts a negative integer and a positive integer" $
      typesShouldBeEqual @TInt @(Neg 3 - Pos 5) @(Neg 8)
    it "correctly subtracts a positive integer and a negative integer" $
      typesShouldBeEqual @TInt @(Pos 7 - Neg 8) @(Pos 15)
    it "correctly subtracts two rationals with a negative result" $
      typesShouldBeEqual @Rat @(2:%3 - 5:%7) @(Neg 1 :% 21)
    it "correctly subtracts two rationals with a positive result" $
      typesShouldBeEqual @Rat @(5:%7 - 2:%3) @(Pos 1 :% 21)
    it "correctly subtracts a rational with a natural" $
      typesShouldBeEqual @Rat @(2:%3 - 2) @(Neg 4 :% 3)
    it "correctly subtracts a natural with a rational" $
      typesShouldBeEqual @Rat @(2 - 2:%3) @(Pos 4 :% 3)
    it "correctly subtracts a rational with a negative integer" $
      typesShouldBeEqual @Rat @(2:%3 - Neg 2) @(Pos 8 :% 3)
    it "correctly subtracts a negative integer with a rational" $
      typesShouldBeEqual @Rat @(Neg 2 - 2:%3) @(Neg 8 :% 3)
    it "correctly subtracts a rational with a positive integer" $
      typesShouldBeEqual @Rat @(2:%3 - Pos 2) @(Neg 4 :% 3)
    it "correctly subtracts a positive integer with a rational" $
      typesShouldBeEqual @Rat @(Pos 2 - 2:%3) @(Pos 4 :% 3)
    it "correctly simplifies the result of subtracting two rationals" $
      typesShouldBeEqual @Rat @(4:%9 - 1:%9) @(Pos 1:%3)

multiplicationTests :: Spec
multiplicationTests =
  describe "multiplication" $ do
    it "correctly multiplies two natural numbers" $
      typesShouldBeEqual @Nat @(3 * 5) @15
    it "correctly multiplies a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Neg 4 * 3) @(Neg 12)
    it "correctly multiplies a natural and a negative integer" $
      typesShouldBeEqual @TInt @(7 * Neg 8) @(Neg 56)
    it "correctly multiplies two negative integers" $
      typesShouldBeEqual @TInt @(Neg 5 * Neg 6) @(Pos 30)
    it "correctly multiplies two positive integers" $
      typesShouldBeEqual @TInt @(Pos 5 * Pos 6) @(Pos 30)
    it "correctly multiplies a negative integer and a positive integer" $
      typesShouldBeEqual @TInt @(Neg 4 * Pos 3) @(Neg 12)
    it "correctly multiplies a positive integer and a negative integer" $
      typesShouldBeEqual @TInt @(Pos 7 * Neg 8) @(Neg 56)
    it "correctly multiplies two rationals" $
      typesShouldBeEqual @Rat @(2:%3 * 5:%7) @(Pos 10 :% 21)
    it "correctly multiplies a rational with a natural" $
      typesShouldBeEqual @Rat @(2:%3 * 2) @(Pos 4 :% 3)
    it "correctly multiplies a natural with a rational" $
      typesShouldBeEqual @Rat @(2 * 2:%3) @(Pos 4 :% 3)
    it "correctly multiplies a rational with a negative integer" $
      typesShouldBeEqual @Rat @(2:%3 * Neg 2) @(Neg 4 :% 3)
    it "correctly multiplies a negative integer with a rational" $
      typesShouldBeEqual @Rat @(Neg 2 * 2:%3) @(Neg 4 :% 3)
    it "correctly multiplies a rational with a positive integer" $
      typesShouldBeEqual @Rat @(2:%3 * Pos 2) @(Pos 4 :% 3)
    it "correctly multiplies a positive integer with a rational" $
      typesShouldBeEqual @Rat @(Pos 2 * 2:%3) @(Pos 4 :% 3)
    it "correctly simplifies the result of multiplying two rationals" $
      typesShouldBeEqual @Rat @(2:%3 * 3:%5) @(Pos 2 :% 5)

absTests :: Spec
absTests = do
  describe "abs" $ do
    it "Acts as a no-op on a natural" $
      typesShouldBeEqual @Nat @(Abs 3) @3
    it "Acts as a no-op on a positive int" $
      typesShouldBeEqual @TInt @(Abs (Pos 3)) @(Pos 3)
    it "Negates a negative int" $
      typesShouldBeEqual @TInt @(Abs (Neg 3)) @(Pos 3)
    it "Normalises a positive rational with natural numerator" $
      typesShouldBeEqual @Rat @(Abs (2:%3)) @(Pos 2 :% 3)
    it "Normalises a positive rational with signed positive numerator" $
      typesShouldBeEqual @Rat @(Abs (Pos 4 :% 6)) @(Pos 2 :% 3)
    it "Negates a negative rational" $
      typesShouldBeEqual @Rat @(Abs (Neg 2 :% 3)) @(Pos 2 :% 3)

negateTests :: Spec
negateTests = do
  describe "negate" $ do
    it "Turns a natural into Neg" $
      typesShouldBeEqual @TInt @(Negate 3) @(Neg 3)
    it "Turns Pos into Neg" $
      typesShouldBeEqual @TInt @(Negate (Pos 3)) @(Neg 3)
    it "Turns Neg into Pos" $
      typesShouldBeEqual @TInt @(Negate (Neg 3)) @(Pos 3)
    it "Negates a rational with natural numerator" $
      typesShouldBeEqual @Rat @(Negate (3 :% 4)) @(Neg 3 :% 4)
    it "Negates a rational with positive numerator" $
      typesShouldBeEqual @Rat @(Negate (Pos 4 :% 5)) @(Neg 4 :% 5)
    it "Negates a rational with negative numerator" $
      typesShouldBeEqual @Rat @(Negate (Neg 5 :% 4)) @(Pos 5 :% 4)

recipTests :: Spec
recipTests = do
  describe "recip" $ do
    it "Inverts Nat / Nat" $
      typesShouldBeEqual @Rat @(Recip (3 :% 2)) @(Pos 2 :% 3)
    it "Inverts Pos / Nat" $
      typesShouldBeEqual @Rat @(Recip (Pos 3 :% 2)) @(Pos 2 :% 3)
    it "Inverts Neg / Nat" $
      typesShouldBeEqual @Rat @(Recip (Neg 3 :% 2)) @(Neg 2 :% 3)
    it "Inverts Nat" $
      typesShouldBeEqual @Rat @(Recip 5) @(Pos 1 :% 5)
    it "Inverts a positive TInt" $
      typesShouldBeEqual @Rat @(Recip (Pos 4)) @(Pos 1 :% 4)
    it "Inverts a negative TInt" $
      typesShouldBeEqual @Rat @(Recip (Neg 3)) @(Neg 1 :% 3)

divisionTests :: Spec
divisionTests = do
  describe "ratdiv" $ do
    it "Divides two naturals" $
      typesShouldBeEqual @Rat @(3 / 2) @(Pos 3 :% 2)
    it "Divides a positive integer by a natural" $
      typesShouldBeEqual @Rat @(Pos 3 / 2) @(Pos 3 :% 2)
    it "Divides a negative integer by a natural" $
      typesShouldBeEqual @Rat @(Neg 3 / 2) @(Neg 3 :% 2)
    it "Divides a natural by a positive integer" $
      typesShouldBeEqual @Rat @(3 / Pos 2) @(Pos 3 :% 2)
    it "Divides a natural by a negative integer" $
      typesShouldBeEqual @Rat @(3 / Neg 2) @(Neg 3 :% 2)
    it "Divides a positive integer by a negative integer" $
      typesShouldBeEqual @Rat @(Pos 3 / Neg 2) @(Neg 3 :% 2)
    it "Divides a negative integer by a positive integer" $
      typesShouldBeEqual @Rat @(Neg 3 / Pos 2) @(Neg 3 :% 2)
    it "Divides a negative integer by a negative integer" $
      typesShouldBeEqual @Rat @(Neg 3 / Neg 2) @(Pos 3 :% 2)
    it "Divides a natural by a rational" $
      typesShouldBeEqual @Rat @(5 / (2:%3)) @(Pos 15 :% 2)
    it "Divides a positive integer by a rational" $
      typesShouldBeEqual @Rat @(Pos 5 / 2:%3) @(Pos 15 :% 2)
    it "Divides a negative integer by a rational" $
      typesShouldBeEqual @Rat @(Neg 5 / 2:%3) @(Neg 15 :% 2)
    it "Divides a negative integer by a negative rational" $
      typesShouldBeEqual @Rat @(Neg 5 / (Neg 2:% 3)) @(Pos 15 :% 2)
    it "Divides a rational by a natural" $
      typesShouldBeEqual @Rat @(2:%3 / 5) @(Pos 2 :% 15)
    it "Divides a rational by a positive integer" $
      typesShouldBeEqual @Rat @(2:%3 / Pos 5) @(Pos 2 :% 15)
    it "Divides a rational by a negative integer" $
      typesShouldBeEqual @Rat @(2:%3 / Neg 5) @(Neg 2 :% 15)
    it "Divides a negative rational by a negative integer" $
      typesShouldBeEqual @Rat @((Neg 2 :% 3) / Neg 5) @(Pos 2 :% 15)
    it "Divides a rational by a rational" $
      typesShouldBeEqual @Rat @(2:%3 / 5:%7) @(Pos 14 :% 15)
    it "Simplifies the result of division" $
      typesShouldBeEqual @Rat @(6 / 4) @(Pos 3 :% 2)

divModTests :: Spec
divModTests = do
  describe "divMod" $ do
    it "calculates divMod for two naturals" $
      typesShouldBeEqual @(Nat,Nat) @(DivMod 29 5) @'(5,4)
    it "calculates divMod for a positive integer and a natural" $
      typesShouldBeEqual @(TInt,TInt) @(DivMod (Pos 29) 5) @'(Pos 5, Pos 4)
    it "calculates divMod for a negative integer and a natural" $
      typesShouldBeEqual @(TInt,TInt) @(DivMod (Neg 29) 5) @'(Neg 6, Pos 1)
    it "calculates divMod for a negative integer and 1" $
      typesShouldBeEqual @(TInt, TInt) @(DivMod (Neg 3) 1) @'(Neg 3, Pos 0)
  describe "div" $ do
    it "calculates div for two naturals" $
      typesShouldBeEqual @Nat @(Div 29 5) @5
    it "calculates div for a positive integer and a natural" $
      typesShouldBeEqual @TInt @(Div (Pos 29) 5) @(Pos 5)
    it "calculates div for a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Div (Neg 29) 5) @(Neg 6)
    it "divides a negative number by 1" $
      typesShouldBeEqual @TInt @(Div (Neg 3) 1) @(Neg 3)
  describe "mod" $ do
    it "calculates mod for two naturals" $
      typesShouldBeEqual @Nat @(Mod 29 5) @4
    it "calculates mod for a positive integer and a natural" $
      typesShouldBeEqual @TInt @(Mod (Pos 29) 5) @(Pos 4)
    it "calculates mod for a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Mod (Neg 29) 5) @(Pos 1)
    it "calculates mod for a negative integer and 1" $
      typesShouldBeEqual @TInt @(Mod (Neg 3) 1) @(Pos 0)

quotRemTests :: Spec
quotRemTests = do
  describe "quotRem" $ do
    it "calculates quotRem for two naturals" $
      typesShouldBeEqual @(Nat,Nat) @(QuotRem 29 5) @'(5,4)
    it "calculates quotRem for a positive integer and a natural" $
      typesShouldBeEqual @(TInt,TInt) @(QuotRem (Pos 29) 5) @'(Pos 5, Pos 4)
    it "calculates quotRem for a negative integer and a natural" $
      typesShouldBeEqual @(TInt,TInt) @(QuotRem (Neg 29) 5) @'(Neg 5, Neg 4)
    it "calculates quotRem for a negative integer and 1" $
      typesShouldBeEqual @(TInt,TInt) @(QuotRem (Neg 3) 1) @'(Neg 3, Pos 0)
  describe "quot" $ do
    it "calculates quot for two naturals" $
      typesShouldBeEqual @Nat @(Quot 29 5) @5
    it "calculates quot for a positive integer and a natural" $
      typesShouldBeEqual @TInt @(Quot (Pos 29) 5) @(Pos 5)
    it "calculates quot for a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Quot (Neg 29) 5) @(Neg 5)
    it "calculates quotRem for a negative integer and 1" $
      typesShouldBeEqual @TInt @(Quot (Neg 3) 1) @(Neg 3)
  describe "rem" $ do
    it "calculates rem for two naturals" $
      typesShouldBeEqual @Nat @(Rem 29 5) @4
    it "calculates rem for a positive integer and a natural" $
      typesShouldBeEqual @TInt @(Rem (Pos 29) 5) @(Pos 4)
    it "calculates rem for a negative integer and a natural" $
      typesShouldBeEqual @TInt @(Rem (Neg 29) 5) @(Neg 4)
    it "calculates rem for a negative integer and 1" $
      typesShouldBeEqual @TInt @(Rem (Neg 3) 1) @(Pos 0)

gcdTests :: Spec
gcdTests = do
  describe "gcd" $ do
    it "calculates gcd for two naturals" $
      typesShouldBeEqual @Nat @(GCD 50 40) @10
    it "calculates gcd for a positive int and a natural" $
      typesShouldBeEqual @Nat @(GCD (Pos 50) 40) @10
    it "calculates gcd for a natural and a positive int" $
      typesShouldBeEqual @Nat @(GCD 50 (Pos 40)) @10
    it "calculates gcd for a negative int and a natural" $
      typesShouldBeEqual @Nat @(GCD (Neg 50) 40) @10
    it "calculates gcd for a natural and a negative int" $
      typesShouldBeEqual @Nat @(GCD 50 (Neg 40)) @10
    it "calculates gcd for two positive integers" $
      typesShouldBeEqual @Nat @(GCD (Pos 50) (Pos 40)) @10
    it "calculates gcd for a positive and a negative int" $
      typesShouldBeEqual @Nat @(GCD (Pos 50) (Neg 40)) @10
    it "calculates gcd for a negative and a positive int" $
      typesShouldBeEqual @Nat @(GCD (Neg 50) (Pos 40)) @10
    it "calculates gcd for two negative integers" $
      typesShouldBeEqual @Nat @(GCD (Neg 50) (Neg 40)) @10

simplifyTests :: Spec
simplifyTests = do
  describe "simplify" $ do
    it "acts as a no-op for an already-simplified fraction" $
      typesShouldBeEqual @Rat @(Simplify (2 :% 3)) @(Pos 2 :% 3)
    it "reduces an unreduced fraction" $
      typesShouldBeEqual @Rat @(Simplify (4 :% 6)) @(Pos 2 :% 3)
    it "no-ops for an already-simplified positive TInt fraction" $
      typesShouldBeEqual @Rat @(Pos 2 :% 3) @(Pos 2 :% 3)
    it "no-opts for an already-simplified negative TInt fraction" $
      typesShouldBeEqual @Rat @(Simplify (Neg 2 :% 3)) @(Neg 2 :% 3)
    it "reduces a positive TInt unreduced fraction" $
      typesShouldBeEqual @Rat @(Simplify (Pos 4 :% 6)) @(Pos 2 :% 3)
    it "reduces a (neg, int) unreduced fraction" $
      typesShouldBeEqual @Rat @(Simplify (Neg 4 :% 6)) @(Neg 2 :% 3)

expTests :: Spec
expTests = do
  describe "exp" $ do
    it "calculates x^0 for natural x" $
      typesShouldBeEqual @Nat @(3^0) @1
    it "calculates x^(+0) for natural x" $
      typesShouldBeEqual @Rat @(3^Pos 0) @(Pos 1 :% 1)
    it "calculates x^(-0) for natural x" $
      typesShouldBeEqual @Rat @(3^Neg 0) @(Pos 1 :% 1)
    it "calculates x^0 for integer x" $
      typesShouldBeEqual @TInt @(Pos 3 ^ 0) @(Pos 1)
    it "calculates x^(+0) for integer x" $
      typesShouldBeEqual @Rat @(Pos 3 ^ Pos 0) @(Pos 1 :% 1)
    it "calculates x^(Neg 0) for integer x" $
      typesShouldBeEqual @Rat @(Pos 3 ^ Neg 0) @(Pos 1 :% 1)
    it "calculates x^0 for rational x" $
      typesShouldBeEqual @Rat @(3:%5 ^ 0) @(Pos 1 :% 1)
    it "calculates x^(+0) for rational x" $
      typesShouldBeEqual @Rat @(3:%5 ^ Pos 0) @(Pos 1 :% 1)
    it "calculates x^(Neg 0) for rational x" $
      typesShouldBeEqual @Rat @(3:%5 ^ Neg 0) @(Pos 1 :% 1)
    it "calculates x^y for natural x,y" $
      typesShouldBeEqual @Nat @(5^3) @125
    it "calculates (+x)^y for natural y" $
      typesShouldBeEqual @TInt @(Pos 5 ^ 3) @(Pos 125)

roundingTests :: Spec
roundingTests = do
  describe "truncate" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual @TInt @(Truncate 3) @(Pos 3)
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual @TInt @(Truncate (Pos 3)) @(Pos 3)
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual @TInt @(Truncate (Neg 3)) @(Neg 3)
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual @TInt @(Truncate (3 :% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual @TInt @(Truncate (Pos 3 :% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual @TInt @(Truncate (Neg 3 :% 1)) @(Neg 3)
    it "rounds a positive rational towards 0" $
      typesShouldBeEqual @TInt @(Truncate (25 :% 3)) @(Pos 8)
    it "rounds a negative rational towards 0" $
      typesShouldBeEqual @TInt @(Truncate (Neg 25 ':% 3)) @(Neg 8)
  describe "floor" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual @TInt @(Floor 3) @(Pos 3)
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual @TInt @(Floor (Pos 3)) @(Pos 3)
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual @TInt @(Floor (Neg 3)) @(Neg 3)
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual @TInt @(Floor (3 :% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual @TInt @(Floor (Pos 3 :% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual @TInt @(Floor (Neg 3 :% 1)) @(Neg 3)
    it "rounds a positive rational towards -inf" $
      typesShouldBeEqual @TInt @(Floor (25 ':% 3)) @(Pos 8)
    it "rounds a negative rational towards -inf" $
      typesShouldBeEqual @TInt @(Floor (Neg 25 ':% 3)) @(Neg 9)
  describe "ceiling" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual @TInt @(Ceiling 3) @(Pos 3)
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual @TInt @(Ceiling (Pos 3)) @(Pos 3)
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual @TInt @(Ceiling (Neg 3)) @(Neg 3)
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual @TInt @(Ceiling (3 ':% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual @TInt @(Ceiling (Pos 3 ':% 1)) @(Pos 3)
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual @TInt @(Ceiling (Neg 3 ':% 1)) @(Neg 3)
    it "rounds a positive rational towards +inf" $
      typesShouldBeEqual @TInt @(Ceiling (25 ':% 3)) @(Pos 9)
    it "rounds a negative rational towards +inf" $
      typesShouldBeEqual @TInt @(Ceiling (Neg 25 ':% 3)) @(Neg 8)

logTests :: Spec
logTests = do
  describe "intlog" $ do
    it "calculates log_2(8)" $
      typesShouldBeEqual @TInt @(IntLog 2 8) @(Pos 3)
    it "calculates log_2(3)" $
      typesShouldBeEqual @TInt @(IntLog 2 3) @(Pos 1)
    it "calculates log_3(8)" $
      typesShouldBeEqual @TInt @(IntLog 3 8) @(Pos 1)
    it "calculates log_3(9)" $
      typesShouldBeEqual @TInt @(IntLog 3 9) @(Pos 2)
    it "calcuclates log_3(2)" $
      typesShouldBeEqual @TInt @(IntLog 3 2) @(Pos 0)
    it "calculates log_2(1/2)" $
      typesShouldBeEqual @TInt @(IntLog 2 (1 :% 2)) @(Neg 1)
    it "calculates log_2(2/3)" $
      typesShouldBeEqual @TInt @(IntLog 2 (2 :% 3)) @(Neg 1)
    it "calculates log_2(1/3)" $
      typesShouldBeEqual @TInt @(IntLog 2 (1 :% 3)) @(Neg 2)
    it "calculates log_3(1/27)" $
      typesShouldBeEqual @TInt @(IntLog 3 (1 :% 27)) @(Neg 3)
    it "calculates log_3(1/2)" $
      typesShouldBeEqual @TInt @(IntLog 3 (1 :% 2)) @(Neg 1)
    it "calculates log_3(3/5)" $
      typesShouldBeEqual @TInt @(IntLog 3 (3 :% 5)) @(Neg 1)
    it "calculates log_6(3/5)" $
      typesShouldBeEqual @TInt @(IntLog 6 (3 :% 5)) @(Neg 1)
