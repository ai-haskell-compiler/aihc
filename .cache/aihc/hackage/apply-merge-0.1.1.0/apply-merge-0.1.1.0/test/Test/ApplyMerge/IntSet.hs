-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.ApplyMerge.IntSet
  ( tests,
  )
where

import ApplyMerge.IntSet (applyMerge)
import Test.ApplyMerge.Common
  ( basicTest,
    blockTest,
    maxTest,
    skewedTest,
  )
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "ApplyMerge.IntSet"
    [ basicTest applyMerge,
      skewedTest applyMerge,
      blockTest applyMerge,
      maxTest applyMerge
    ]
