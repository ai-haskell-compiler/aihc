module Data.Hash.Murmur.Units (tests) where

import Test.HUnit (assertBool, Assertion)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.Word (Word32)
import qualified Data.ByteString.Base16 as B16 (decodeBase16Lenient)
import qualified Data.ByteString.Char8 as C (pack)

import Data.Hash.Murmur (murmur3)

type TestVector = (Word32, Word32, String)

tests :: [Test]
tests =
    [ testGroup "MurmurHash3"
        (map buildTest $ zip testVectors [0..])
    ]

buildTest :: (TestVector, Int) -> Test.Framework.Test
buildTest (v, i) =
    testCase ("MurmurHash3 test vector " ++ show i) $ assertTestVector v

assertTestVector :: TestVector -> Assertion
assertTestVector (expected, seed, str) =
    assertBool "    > MurmurHash3 " $ result == expected
  where
    result = murmur3 seed (B16.decodeBase16Lenient $ C.pack str)

testVectors :: [TestVector]
testVectors =
    [ (0x00000000, 0x00000000, "")
    , (0x6a396f08, 0xFBA4C795, "")
    , (0x81f16f39, 0xffffffff, "")
    , (0x514e28b7, 0x00000000, "00")
    , (0xea3f0b17, 0xFBA4C795, "00")
    , (0xfd6cf10d, 0x00000000, "ff")
    , (0x16c6b7ab, 0x00000000, "0011")
    , (0x8eb51c3d, 0x00000000, "001122")
    , (0xb4471bf8, 0x00000000, "00112233")
    , (0xe2301fa8, 0x00000000, "0011223344")
    , (0xfc2e4a15, 0x00000000, "001122334455")
    , (0xb074502c, 0x00000000, "00112233445566")
    , (0x8034d2a0, 0x00000000, "0011223344556677")
    , (0xb4698def, 0x00000000, "001122334455667788")
    ]

