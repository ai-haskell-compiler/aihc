-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.ApplyMerge.MergeAll
  ( tests,
  )
where

import ApplyMerge.MergeAll (applyMerge)
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
    "ApplyMerge.MergeAll"
    [ basicTest applyMerge,
      skewedTest applyMerge,
      blockTest applyMerge,
      maxTest applyMerge
    ]
