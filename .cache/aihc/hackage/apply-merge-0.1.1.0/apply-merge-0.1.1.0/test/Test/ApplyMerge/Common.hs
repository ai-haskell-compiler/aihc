-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE Rank2Types #-}

module Test.ApplyMerge.Common
  ( basicTest,
    skewedTest,
    blockTest,
    maxTest,
  )
where

import Data.List (sort)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type ApplyMerge = forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]

basicTest :: ApplyMerge -> TestTree
basicTest applyMerge =
  testGroup
    "basic tests"
    [ testCase "applyMerge (+) [] [] == []" $
        applyMerge (undefined :: Int -> Int -> Int) [] [] @?= [],
      testCase "applyMerge (+) [0 ..] [] == []" $
        applyMerge (+) ([0 ..] :: [Int]) [] @?= [],
      testCase "applyMerge (+) [] [0 ..] == []" $
        applyMerge (+) ([] :: [Int]) [0 ..] @?= [],
      testCase "applyMerge (+) [0 .. 10] [0 .. 10] is correct" $
        let xs :: [Int]
            xs = [0 .. 10]
            expected = sort $ (+) <$> xs <*> xs
         in applyMerge (+) xs xs @?= expected,
      testCase "applyMerge (+) [0 .. 10] (replicate 10 1) is correct" $
        let xs :: [Int]
            xs = [0 .. 10]
            ys :: [Int]
            ys = replicate 10 1
            expected = sort $ (+) <$> xs <*> ys
         in applyMerge (+) xs ys @?= expected,
      testCase "applyMerge (+) (replicate 10 1) [0 .. 10] is correct" $
        let xs :: [Int]
            xs = [0 .. 10]
            ys :: [Int]
            ys = replicate 10 1
            expected = sort $ (+) <$> ys <*> xs
         in applyMerge (+) ys xs @?= expected,
      testCase "applyMerge (+) (replicate 10 1) (replicate 10 1) is correct" $
        let ys :: [Int]
            ys = replicate 10 1
            expected = sort $ (+) <$> ys <*> ys
         in applyMerge (+) ys ys @?= expected,
      testCase "applyMerge (+) [0..] [0..] is correct in the first 100 elems" $
        let xs :: [Int]
            xs = [0 ..]
            expected :: [Int]
            expected = take 100 $ sort $ (+) <$> take 100 xs <*> take 100 xs
         in take 100 (applyMerge (+) xs xs) @?= expected
    ]

skewedTest :: ApplyMerge -> TestTree
skewedTest applyMerge =
  testGroup
    "skewed tests"
    [ testCase "applyMerge (^) [2..] [1..] is correct in the first 100 elems" $
        let bases :: [Integer]
            bases = [2 ..]
            exps :: [Int]
            exps = [1 ..]
            expected :: [Integer]
            expected = take 100 $ sort $ (^) <$> take 100 bases <*> take 100 exps
         in take 100 (applyMerge (^) bases exps) @?= expected,
      testCase "applyMerge (flip (^)) [2..] [1..] is correct in the first 100 elems" $
        let bases :: [Integer]
            bases = [2 ..]
            exps :: [Int]
            exps = [1 ..]
            expected :: [Integer]
            expected = take 100 $ sort $ flip (^) <$> take 100 exps <*> take 100 bases
         in take 100 (applyMerge (flip (^)) exps bases) @?= expected
    ]

blockTest :: ApplyMerge -> TestTree
blockTest applyMerge =
  testGroup
    "block tests"
    [ testCase
        ( "applyMerge (\\x y -> (x `quot` 3) + (y `quot` 3)) [0..] [0..] is correct in the "
            ++ "first 100 elems"
        )
        $ let xs :: [Int]
              xs = [0 ..]
              f :: Int -> Int -> Int
              f x y = (x `quot` 3) + (y `quot` 3)
              expected :: [Int]
              expected = take 100 $ sort $ f <$> take 100 xs <*> take 100 xs
           in take 100 (applyMerge f xs xs) @?= expected
    ]

maxTest :: ApplyMerge -> TestTree
maxTest applyMerge =
  testGroup
    "max tests"
    [ testCase "applyMerge max [0..] [0..] is correct in the first 100 elems" $
        let xs :: [Int]
            xs = [0 ..]
            expected :: [Int]
            expected = take 100 $ sort $ max <$> take 100 xs <*> take 100 xs
         in take 100 (applyMerge max xs xs) @?= expected
    ]
