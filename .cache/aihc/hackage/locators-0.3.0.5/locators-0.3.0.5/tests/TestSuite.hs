{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-unused-imports #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module TestSuite where

import Control.Exception (evaluate)
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (elements, property)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.Map.Strict as Map
import Debug.Trace

--
-- What we're actually testing.
--

import Data.Locator

suite :: Spec
suite = do
    describe "Locators (English16)" $ do
        testRoundTripEnglish16
        testKnownEnglish16a
        testProblematicEdgeCases
        testNegativeNumbers
        testWidthGuardsEnglish16a

    describe "Locators (Latin25)" $ do
        testKnownLatin25
        testRoundTripLatin25
        testHashLatin25
        testWidthGuardsHashing

    describe "Hashes" $ do
        testPaddingRefactored

testRoundTripEnglish16 =
    prop "safe conversion to/from English16" prop_English16

prop_English16 :: Int -> Bool
prop_English16 i =
    let n = abs i
        decoded = fromEnglish16 (toEnglish16 n)
     in n == decoded

--
-- Have to do these manually, since Locator16a is not round-trip safe.
--
testKnownEnglish16a =
    it "constrains English16a to unique digits" $ do
        toEnglish16a 6 0x111111 `shouldBe` "12C4FH"
        toEnglish16a 6 0x777777 `shouldBe` "789KLM"
        toEnglish16a 6 0xCCCCCC `shouldBe` "MRXY01"

testProblematicEdgeCases =
    it "converstion to Locator16a correct on corner cases" $ do
        toEnglish16a 6 0x0 `shouldBe` "012C4F"
        hashStringToEnglish16a 6 "perf_data" `shouldBe` "FHL417"
        hashStringToEnglish16a 6 "perf_data/bletchley" `shouldBe` "K48F01"

testPaddingRefactored =
    it "correctly pads strings" $ do
        padWithZeros 5 "1" `shouldBe` "00001"
        padWithZeros 5 "123456" `shouldBe` "123456"
        (padWithZeros 11 . toBase62 $ 2 ^ 64) `shouldBe` "LygHa16AHYG"
        (hashStringToBase62 11 . S.pack . show $ 2 ^ 64) `shouldBe` "k8SQgkJtxLo"

testNegativeNumbers =
    it "doesn't explode if fed a negative number" $ do
        toEnglish16a 1 (-1) `shouldBe` "1"

testKnownLatin25 =
    it "base 26 is correct" $ do
        toLatin25 0 `shouldBe` "0"
        toLatin25 1 `shouldBe` "1"
        toLatin25 24 `shouldBe` "Z"
        toLatin25 25 `shouldBe` "10"

testRoundTripLatin25 =
    prop "safe conversion to/from Latin25" prop_English16

prop_Latin25 :: Int -> Bool
prop_Latin25 i =
    let n = abs i
        encoded = toLatin25 n
        decoded = fromLatin25 encoded
     in n == decoded

testHashLatin25 =
    it "hashToLatin25 generates an appropriate hash" $ do
        hashStringToLatin25 5 "You'll get used to it. Or, you'll have a psychotic episode"
            `shouldBe` "XSAV1"

testWidthGuardsEnglish16a =
    it "errors if asking for more than 16 English16a characters" $ do
        evaluate (toEnglish16a 17 1) `shouldThrow` anyErrorCall

testWidthGuardsHashing =
    it "errors if asking for more than 17 hash digits" $ do
        S.length (hashStringToLatin25 17 "a") `shouldBe` 17
        evaluate (hashStringToLatin25 18 "a") `shouldThrow` anyErrorCall
