-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.ApplyMerge.IntMap
  ( tests,
  )
where

import ApplyMerge.IntMap (applyMerge)
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
    "ApplyMerge.IntMap"
    [ basicTest applyMerge,
      skewedTest applyMerge,
      blockTest applyMerge,
      maxTest applyMerge
    ]
