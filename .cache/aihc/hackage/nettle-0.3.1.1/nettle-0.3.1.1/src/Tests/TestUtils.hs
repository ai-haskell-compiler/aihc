
module TestUtils
	( defaultMain
	, testGroup
	, Test(..)

	, assertEqual
	, Assertion
	, testCase

	, testCases
	, debugTestCases

	, assertEqualHex

	, module HexUtils
	, module Data.Tagged
	) where

import HexUtils

import Test.Framework (defaultMain, testGroup, Test(..))
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, assertBool, Assertion)

import Data.Tagged
import qualified Data.ByteString as B
import qualified Data.Array.IArray as A

testCases :: String -> [Assertion] -> Test
testCases title = testCase title . mapM_ id

debugTestCases :: String -> [Assertion] -> Test
debugTestCases title = testGroup title . zipWith (\n c -> testCase (show n) c) [1..]

assertEqualHex :: String -> B.ByteString -> B.ByteString -> Assertion
assertEqualHex preface expected actual = assertBool msg (actual == expected) where
	msg = (if null preface then "" else preface ++ "\n") ++ "\texpected: " ++ hexs expected ++ "\n\t but got: " ++ hexs actual
