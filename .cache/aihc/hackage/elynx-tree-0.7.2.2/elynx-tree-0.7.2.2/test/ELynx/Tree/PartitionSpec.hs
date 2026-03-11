{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.PartitionSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 09:38:50 2019.
module ELynx.Tree.PartitionSpec
  ( spec,
  )
where

import Data.Set (Set, fromList)
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec

ex1 :: Tree () Int
ex1 = Node () 0 [Node () 1 [], Node () 2 [Node () 4 [], Node () 5 [], Node () 6 []], Node () 3 []]

sol1 :: Set (Partition Int)
sol1 =
  fromList
    [ ptUnsafe [fromList [1], fromList [3], fromList [4, 5, 6]],
      ptUnsafe [fromList [1, 3], fromList [4], fromList [5], fromList [6]]
    ]

ex2 :: Tree () Int
ex2 = Node () 0 [Node () 1 [], Node () 2 [], Node () 0 [Node () 3 [], Node () 4 []], Node () 5 []]

sol2 :: Set (Partition Int)
sol2 =
  fromList
    [ ptUnsafe [fromList [1], fromList [2], fromList [3, 4], fromList [5]],
      ptUnsafe [fromList [1, 2, 5], fromList [3], fromList [4]]
    ]

spec :: Spec
spec = describe "partitions" $
  it "calculates correct partitions for a sample tree" $
    do
      -- t <- removeBrInfo <$> getMultifurcatingTree
      -- print $ partitions ex1
      partitions ex1 `shouldBe` Right sol1
      partitions ex2 `shouldBe` Right sol2

-- it "is empty for a collection of random bifurcating trees"
--   $ property (prop_bifurcating_tree :: Tree (PhyloLabel Double) -> Bool)
