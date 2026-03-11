-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Bench.PriorityQueue.MinPQueue (benchmarks) where

import Data.List (foldl')
import Data.PQueue.Prio.Min (deleteMin, fromAscList, getMin, insert)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

generate :: Int -> [Int]
generate = iterate (\x -> 6364136223846793005 * x + 1442695040888963407)

insertDeleteBenchmark :: Benchmark
insertDeleteBenchmark = bgroup "insert/delete" (map mkBench [1 .. 6])
  where
    mkBench :: Int -> Benchmark
    mkBench i =
      let quantity :: String
          quantity = "10^" ++ show i
          message :: String
          message =
            quantity
              ++ " inserts and deletes on a priority queue of size "
              ++ quantity
       in bench message $ flip nf ((10 :: Int) ^ i) $ \n ->
            let initialQueue = fromAscList (map (\x -> (x, x)) [1 .. n])
                finalQueue =
                  foldl'
                    (\pq (x :: Int) -> deleteMin (insert x x pq))
                    initialQueue
                    (take n (generate n))
             in getMin finalQueue

benchmarks :: Benchmark
benchmarks = bgroup "Bench.PriorityQueue.MinPQueue" [insertDeleteBenchmark]
