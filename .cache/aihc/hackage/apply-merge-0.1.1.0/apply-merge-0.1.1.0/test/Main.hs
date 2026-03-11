-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Test.ApplyMerge.DoublyLinkedList qualified (tests)
import Test.ApplyMerge.IntMap qualified (tests)
import Test.ApplyMerge.IntSet qualified (tests)
import Test.ApplyMerge.MergeAll qualified (tests)
import Test.Data.DoublyLinkedList.STRef qualified (tests)
import Test.Data.List.ApplyMerge qualified (tests)
import Test.Data.PQueue.Prio.Min.Mutable qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ Test.ApplyMerge.DoublyLinkedList.tests,
      Test.ApplyMerge.IntMap.tests,
      Test.ApplyMerge.IntSet.tests,
      Test.ApplyMerge.MergeAll.tests,
      Test.Data.List.ApplyMerge.tests,
      Test.Data.DoublyLinkedList.STRef.tests,
      Test.Data.PQueue.Prio.Min.Mutable.tests
    ]
