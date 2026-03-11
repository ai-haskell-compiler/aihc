-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.StringParsing (suite_StringParsing) where

import Options
import Test.Hspec

data StringOptions = StringOptions
  { optString :: String,
    optString_defA :: String,
    optString_defU :: String
  }

instance Options StringOptions where
  defineOptions =
    pure StringOptions
      <*> simpleOption "string" "" ""
      -- String, ASCII default
      <*> simpleOption "string_defA" "a" ""
      -- String, Unicode default
      <*> simpleOption "string_defU" "\12354" ""

suite_StringParsing :: Spec
suite_StringParsing = context "string-parsing" do
     test_Defaults
     test_Ascii
     test_UnicodeValid
     test_UnicodeInvalid


test_Defaults :: Spec
test_Defaults = specify "defaults" $ do
  let opts = defaultOptions

  shouldBe (optString_defA opts) "a"
  shouldBe (optString_defU opts) "\12354"

test_Ascii :: Spec
test_Ascii = specify "ascii" $ do
  let parsed = parseOptions ["--string=a"]
  let Just opts = parsedOptions parsed

  shouldBe (optString opts) "a"

test_UnicodeValid :: Spec
test_UnicodeValid = specify "unicode-valid" $ do
  let parsed = parseOptions ["--string=\12354"]
  let Just opts = parsedOptions parsed

  shouldBe (optString opts) "\12354"

test_UnicodeInvalid :: Spec
test_UnicodeInvalid = specify "unicode-invalid" $ do
  let parsed = parseOptions ["--string=\56507"]
  let expectedString = "\56507"
  let Just opts = parsedOptions parsed

  shouldBe (optString opts) expectedString
