-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Bench.ApplyMerge qualified
import Bench.Data.DoublyLinkedList.STRef qualified
import Bench.PriorityQueue.MinPQueue qualified
import Bench.PriorityQueue.MinPQueue.Mutable qualified
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.ApplyMerge.benchmarks,
      Bench.Data.DoublyLinkedList.STRef.benchmarks,
      Bench.PriorityQueue.MinPQueue.benchmarks,
      Bench.PriorityQueue.MinPQueue.Mutable.benchmarks
    ]
