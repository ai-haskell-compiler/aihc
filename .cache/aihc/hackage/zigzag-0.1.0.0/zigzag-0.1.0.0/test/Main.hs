{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Word.Zigzag

import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck (Arbitrary (..), suchThat, testProperty, (===))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "zigzag"
    [ testGroup
        "inverses"
        [ testProperty "to-from-bigint" $ \i ->
            (fromZigzag . toZigzag) i === i
        , testProperty "from-to-bigint" $ \i ->
            (toZigzag . fromZigzag) i === i
        , testProperty "to-from-int" $ \i ->
            (fromZigzagNative . toZigzagNative) i === i
        , testProperty "from-to-int" $ \i ->
            (toZigzagNative . fromZigzagNative) i === i
        , testProperty "to-from-i32" $ \i ->
            (fromZigzag32 . toZigzag32) i === i
        , testProperty "from-to-i32" $ \i ->
            (toZigzag32 . fromZigzag32) i === i
        , testProperty "to-from-i64" $ \i ->
            (fromZigzag64 . toZigzag64) i === i
        , testProperty "from-to-i64" $ \i ->
            (toZigzag64 . fromZigzag64) i === i
        ]
    , testGroup
        "spec-examples"
        [ testGroup
            "bigint"
            [ testCase "0" $ toZigzag 0 @=? 0
            , testCase "1" $ toZigzag 1 @=? 2
            , testCase "-1" $ toZigzag (-1) @=? 1
            , testCase "-2" $ toZigzag (-2) @=? 3
            , testCase "2147483647" $ toZigzag 2147483647 @=? 4294967294
            , testCase "-2147483648" $ toZigzag (-2147483648) @=? 4294967295
            ]
        , testGroup
            "native"
            [ testCase "0" $ toZigzagNative 0 @=? 0
            , testCase "1" $ toZigzagNative 1 @=? 2
            , testCase "-1" $ toZigzagNative (-1) @=? 1
            , testCase "-2" $ toZigzagNative (-2) @=? 3
            , testCase "2147483647" $ toZigzagNative 2147483647 @=? 4294967294
            , testCase "-2147483648" $ toZigzagNative (-2147483648) @=? 4294967295
            ]
        , testGroup
            "i32"
            [ testCase "0" $ toZigzag32 0 @=? 0
            , testCase "1" $ toZigzag32 1 @=? 2
            , testCase "-1" $ toZigzag32 (-1) @=? 1
            , testCase "-2" $ toZigzag32 (-2) @=? 3
            , testCase "2147483647" $ toZigzag32 2147483647 @=? 4294967294
            , testCase "-2147483648" $ toZigzag32 (-2147483648) @=? 4294967295
            ]
        , testGroup
            "i64"
            [ testCase "0" $ toZigzag64 0 @=? 0
            , testCase "1" $ toZigzag64 1 @=? 2
            , testCase "-1" $ toZigzag64 (-1) @=? 1
            , testCase "-2" $ toZigzag64 (-2) @=? 3
            , testCase "2147483647" $ toZigzag64 2147483647 @=? 4294967294
            , testCase "-2147483648" $ toZigzag64 (-2147483648) @=? 4294967295
            ]
        ]
    ]

instance Arbitrary Natural where
  arbitrary = fromIntegral @Integer @Natural <$> arbitrary `suchThat` (>= 0)
  shrink =
    fmap (fromIntegral @Integer @Natural)
      . shrink
      . fromIntegral @Natural @Integer
