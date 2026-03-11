-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Bench.PriorityQueue.MinPQueue.Mutable (benchmarks) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.PQueue.Prio.Min.Mutable (deleteMin, fromAscList, insert)
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
       in bench message $ flip nf ((10 :: Int) ^ i) $ \n -> runST $ do
            queue <- fromAscList (map (\x -> (x, x)) [1 .. n])
            forM_ (take n (generate n)) $ \x -> do
              insert x x queue
              deleteMin queue

            deleteMin queue

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Bench.PriorityQueue.MinPQueue.Mutable"
    [insertDeleteBenchmark]
