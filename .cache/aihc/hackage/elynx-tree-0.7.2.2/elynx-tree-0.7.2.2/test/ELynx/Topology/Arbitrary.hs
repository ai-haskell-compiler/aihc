{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  ELynx.Topology.Arbitrary
-- Description :  Arbitrary instances for topologies
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Jul 22 21:02:46 2021.
module ELynx.Topology.Arbitrary
  (
  )
where

import qualified Data.List.NonEmpty as N
import Data.Traversable
import ELynx.Topology
import Test.QuickCheck

instance Arbitrary1 Topology where
  liftArbitrary arbL = go
    where
      go = sized $ \n -> do
        pars <- frequency [(1, pure [1, 1]), (4, arbPartition (n - 1))]
        case pars of
          [] -> Leaf <$> arbL
          xs -> do
            frst <- for xs $ \i -> resize i go
            return $ Node $ N.fromList frst
      arbPartition :: Int -> Gen [Int]
      arbPartition k = case compare k 1 of
        LT -> pure []
        EQ -> pure [1]
        GT -> do
          first <- elements [1 .. k]
          rest <- arbPartition $ k - first
          return $ first : rest

instance Arbitrary a => Arbitrary (Topology a) where
  arbitrary = arbitrary1

instance (CoArbitrary a) => CoArbitrary (Topology a) where
  coarbitrary (Node frst) = coarbitrary $ N.toList frst
  coarbitrary (Leaf val) = coarbitrary val
