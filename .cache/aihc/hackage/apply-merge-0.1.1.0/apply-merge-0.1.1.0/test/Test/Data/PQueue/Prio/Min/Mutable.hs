-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.PQueue.Prio.Min.Mutable (tests) where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST (runST)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.PQueue.Prio.Min.Mutable
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Data.PQueue.Prio.Min.Mutable"
    [ testProperty "priority queue sorts a list" prop_sorts_list
    ]

prop_sorts_list :: [Int] -> Bool
prop_sorts_list xs = pqueueSort xs == sort xs

pqueueSort :: (Ord a) => [a] -> [a]
pqueueSort xs = runST $ do
  queue <- empty
  forM_ xs $ \x -> insert x () queue
  map fst . catMaybes <$> replicateM (length xs) (deleteMin queue)
