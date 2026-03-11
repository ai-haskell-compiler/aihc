-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImpredicativeTypes #-}

module Bench.ApplyMerge (benchmarks) where

import ApplyMerge.DoublyLinkedList qualified
import ApplyMerge.IntMap qualified
import ApplyMerge.IntSet qualified
import ApplyMerge.MergeAll qualified
import Data.Function ((&))
import Data.List.Ordered (minus)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "ApplyMerge"
    [ funcToBenchmark
        "linear shape: applyMerge const [1..] [1..]"
        const,
      funcToBenchmark
        "double linear shape: applyMerge min [1..] [1..]"
        min,
      funcToBenchmark
        "box shape: applyMerge max [1..] [1..]"
        max,
      funcToBenchmark
        "triangular shape: applyMerge (+) [1..] [1..]"
        (+),
      funcToBenchmark
        "skewed triangular shape: applyMerge (\\x y -> 4 * x + y) [1..] [1..]"
        (\x y -> 4 * x + y),
      funcToBenchmark
        "hyperbolic shape: applyMerge (*) [1..] [1..]"
        (*),
      funcToBenchmark
        "skewed hyperbolic shape: applyMerge (\\x y -> x^3 * y) [1..] [1..]"
        ( \x y ->
            floor @Double $
              100 * (3 * log (fromIntegral x + fromIntegral y))
        ),
      funcToBenchmark
        "circular shape: applyMerge (\\x y -> x*x + y*y) [1..] [1..]"
        (\x y -> x * x + y * y),
      funcToBenchmark
        "elliptical shape: applyMerge (\\x y -> 4*x*x + y*y) [1..] [1..]"
        (\x y -> 4 * x * x + y * y),
      collapseToBenchmark "primes" $ \applyMerge n ->
        let zero :: Int
            zero = (n `quot` maxBound)

            primes :: [Int]
            primes = 2 : 3 : 5 : ([7 ..] `minus` composites)

            composites :: [Int]
            composites = applyMerge (\p j -> p * (p + j)) primes [zero ..]
         in sum (takeWhile (<= n) primes)
    ]

type ApplyMerge = forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]

funcToBenchmark :: String -> (Int -> Int -> Int) -> Benchmark
funcToBenchmark name f = collapseToBenchmark name (funcToCollapse f)

funcToCollapse :: (Int -> Int -> Int) -> ApplyMerge -> Int -> Int
funcToCollapse f applyMerge n =
  let one = (n `quot` maxBound) + 1
   in applyMerge f [one ..] [one ..]
        & take n
        & sum

collapseToBenchmark :: String -> (ApplyMerge -> Int -> Int) -> Benchmark
collapseToBenchmark name collapse = bgroup name (map mkBench [1 .. 6])
  where
    mkBench :: Int -> Benchmark
    mkBench i =
      let limit :: Int
          limit = 10 ^ i

          applyMerges :: [(String, ApplyMerge)]
          applyMerges =
            [ ("DoublyLinkedList", ApplyMerge.DoublyLinkedList.applyMerge),
              ("IntMap", ApplyMerge.IntMap.applyMerge),
              ("IntSet", ApplyMerge.IntSet.applyMerge),
              ("MergeAll", ApplyMerge.MergeAll.applyMerge),
              ( "MergeAll (flipped)",
                flip . ApplyMerge.MergeAll.applyMerge . flip
              )
            ]
       in bgroup (show limit) $
            flip map applyMerges $ \(applyMergeName, applyMerge) ->
              bench applyMergeName (nf (collapse applyMerge) limit)
