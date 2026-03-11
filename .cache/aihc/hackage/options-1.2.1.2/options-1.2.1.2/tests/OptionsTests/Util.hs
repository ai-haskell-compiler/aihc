-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Util (suite_Util) where

import Options.Util
import Test.Hspec

suite_Util :: Spec
suite_Util = context "util" do
  test_ValidFieldName
  test_ValidShortFlag
  test_ValidLongFlag
  test_HasDuplicates

test_ValidFieldName :: Spec
test_ValidFieldName = specify "validFieldName" do
  shouldBe True $ validFieldName "a"
  shouldBe True $ validFieldName "abc"
  shouldBe True $ validFieldName "_abc_"
  shouldBe True $ validFieldName "abc'"
  shouldBe True $ validFieldName "\12354"
  shouldBe False $ validFieldName ""
  shouldBe False $ validFieldName "'a"
  shouldBe False $ validFieldName "a b"
  shouldBe False $ validFieldName "Ab"

test_ValidShortFlag :: Spec
test_ValidShortFlag = specify "validShortFlag" do
  shouldBe True $ validShortFlag 'a'
  shouldBe True $ validShortFlag 'A'
  shouldBe True $ validShortFlag '0'
  shouldBe True $ validShortFlag '\12354'
  shouldBe False $ validShortFlag ' '
  shouldBe False $ validShortFlag '-'

test_ValidLongFlag :: Spec
test_ValidLongFlag = specify "validLongFlag" do
  shouldBe True $ validLongFlag "a"
  shouldBe True $ validLongFlag "A"
  shouldBe True $ validLongFlag "abc"
  shouldBe True $ validLongFlag "0"
  shouldBe True $ validLongFlag "012"
  shouldBe True $ validLongFlag "a-b"
  shouldBe True $ validLongFlag "a_b"
  shouldBe True $ validLongFlag "\12354bc"
  shouldBe False $ validLongFlag ""
  shouldBe False $ validLongFlag "a b"
  shouldBe False $ validLongFlag "a+b"
  shouldBe False $ validLongFlag "-"
  shouldBe False $ validLongFlag "--"

test_HasDuplicates :: Spec
test_HasDuplicates = specify "hasDuplicates" do
  shouldBe False $ hasDuplicates ([] :: [Char])
  shouldBe False $ hasDuplicates ['a', 'b']
  shouldBe True $ hasDuplicates ['a', 'b', 'a']
