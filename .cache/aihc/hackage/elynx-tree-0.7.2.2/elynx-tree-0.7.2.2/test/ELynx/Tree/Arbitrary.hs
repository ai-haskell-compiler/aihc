{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  ELynx.Tree.Arbitrary
-- Description :  Arbitrary instances for trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 21 17:13:23 2020.
module ELynx.Tree.Arbitrary
  (
  )
where

import Data.Traversable
import ELynx.Tree
import Test.QuickCheck

-- Of course, the boundaries for branch support and length have been chosen
-- pretty arbitrarily :).
--
-- NOTE: This instance does not produce values without branch lengths nor branch
-- supports.
instance Arbitrary Phylo where
  arbitrary =
    Phylo
      <$> (Just . toLengthUnsafe <$> choose (0.001 :: Double, 10))
      <*> (Just . toSupportUnsafe . fromIntegral <$> choose (1 :: Int, 100))

instance Arbitrary2 Tree where
  liftArbitrary2 arbB arbN = go
    where
      go = sized $ \n -> do
        -- Sized is the size of the trees.
        br <- arbB
        val <- arbN
        pars <- frequency [(1, pure [1, 1]), (1, pure [1]), (4, arbPartition (n - 1))]
        frst <- for pars $ \i -> resize i go
        return $ Node br val frst
      arbPartition :: Int -> Gen [Int]
      arbPartition k = case compare k 1 of
        LT -> pure []
        EQ -> pure [1]
        GT -> do
          first <- elements [1 .. k]
          rest <- arbPartition $ k - first
          return $ first : rest

instance (Arbitrary e, Arbitrary a) => Arbitrary (Tree e a) where
  arbitrary = arbitrary2

instance (CoArbitrary e, CoArbitrary a) => CoArbitrary (Tree e a) where
  coarbitrary (Node br val frst) =
    coarbitrary br . coarbitrary val . coarbitrary frst

instance Arbitrary Length where
  arbitrary = toLengthUnsafe . getPositive <$> arbitrary

instance Arbitrary2 BranchTree where
  liftArbitrary2 ga gb = BranchTree <$> liftArbitrary2 gb ga

instance (Arbitrary a, Arbitrary e) => Arbitrary (BranchTree a e) where
  arbitrary = arbitrary2

instance (CoArbitrary a, CoArbitrary e) => CoArbitrary (BranchTree a e) where
  coarbitrary (BranchTree (Node br val frst)) =
    coarbitrary br . coarbitrary val . coarbitrary frst

instance Arbitrary2 ZipTree where
  liftArbitrary2 ga gb = ZipTree <$> liftArbitrary2 ga gb

instance (Arbitrary e, Arbitrary a) => Arbitrary (ZipTree e a) where
  arbitrary = arbitrary2

instance (CoArbitrary e, CoArbitrary a) => CoArbitrary (ZipTree e a) where
  coarbitrary (ZipTree (Node br val frst)) =
    coarbitrary br . coarbitrary val . coarbitrary frst

instance Arbitrary2 ZipBranchTree where
  liftArbitrary2 ga gb = ZipBranchTree <$> liftArbitrary2 gb ga

instance (Arbitrary a, Arbitrary e) => Arbitrary (ZipBranchTree a e) where
  arbitrary = arbitrary2

instance (CoArbitrary a, CoArbitrary e) => CoArbitrary (ZipBranchTree a e) where
  coarbitrary (ZipBranchTree (Node br val frst)) =
    coarbitrary br . coarbitrary val . coarbitrary frst
