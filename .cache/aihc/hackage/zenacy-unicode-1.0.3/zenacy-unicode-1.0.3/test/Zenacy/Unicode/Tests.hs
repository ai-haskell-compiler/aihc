{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zenacy.Unicode.Tests
  ( testUnicode
  ) where

import Zenacy.Unicode
import Data.Monoid
  ( (<>)
  )
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )

testUnicode :: Test
testUnicode = testGroup "Zenacy.Unicode"
  [ testBom
  , testClean
  ]

testBom :: Test
testBom = testCase "unicode bom" $ do
  assertEqual "TEST 1" (Nothing, "a") $ bomStrip "a"
  assertEqual "TEST 2" (Just BOM_UTF8, "abc") $ bomStrip (utf8 <> "abc")
  assertEqual "TEST 3" (Just BOM_UTF16_BE, "abc") $ bomStrip (utf16be <> "abc")
  assertEqual "TEST 4" (Just BOM_UTF16_LE, "abc") $ bomStrip (utf16le <> "abc")
  assertEqual "TEST 5" (Just BOM_UTF32_BE, "abc") $ bomStrip (utf32be <> "abc")
  assertEqual "TEST 6" (Just BOM_UTF32_LE, "abc") $ bomStrip (utf32le <> "abc")
  where
    utf8 = "\xEF\xBB\xBF"
    utf16be = "\xFE\xFF"
    utf16le = "\xFF\xFE"
    utf32be = "\x00\x00\xFE\xFF"
    utf32le = "\xFF\xFE\x00\x00"

testClean :: Test
testClean = testCase "unicode clean" $ do
  assertEqual "TEST 1" "a" $ unicodeCleanUTF8 "a"
  assertEqual "TEST 2" "abc" $ unicodeCleanUTF8 "abc"
  assertEqual "TEST 3" "\xEF\xBF\xBD" $ unicodeCleanUTF8 "\x00"
  assertEqual "TEST 4" "\xEF\xBF\xBD" $ unicodeCleanUTF8 "\x92"
  assertEqual "TEST 5"
    "government\xEF\xBF\xBDs arguments" $
    unicodeCleanUTF8 "government\x92s arguments"
