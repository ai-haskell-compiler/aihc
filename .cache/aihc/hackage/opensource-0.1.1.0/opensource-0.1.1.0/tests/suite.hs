-- suite.hs: opensource library test suite
-- Copyright © 2016  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure, assertEqual)

import Network.Protocol.OpenSource.License (OSILicense)

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
  testGroup "Serialization group" [
     testCase "Parse JSON" (testJSONInput 90 "licenses.json")
     ]
  ]

testJSONInput :: Int -> FilePath -> Assertion
testJSONInput count fp = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    case eitherDecode bs :: Either String [OSILicense] of
        Left e -> assertFailure e
        Right j -> assertEqual ("List count matches " ++ show count) count (length j)

main :: IO ()
main = defaultMain tests
