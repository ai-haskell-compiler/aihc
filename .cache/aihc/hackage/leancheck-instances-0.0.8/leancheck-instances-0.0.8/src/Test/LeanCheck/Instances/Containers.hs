-- |
-- Module      : Test.LeanCheck.Instances.Containers
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- 'Listable' containers.
module Test.LeanCheck.Instances.Containers () where

import Test.LeanCheck

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

instance (Ord a, Listable a) => Listable (Set a) where
  tiers = setCons Set.fromList

instance (Ord a, Ord b, Listable a, Listable b) => Listable (Map a b) where
  tiers = mapCons Map.fromList

instance (Ord a, Listable a) => Listable (Seq a) where
  tiers = setCons Seq.fromList

instance (Ord a, Listable a) => Listable (IntMap a) where
  tiers = mapCons IntMap.fromList

instance Listable IntSet where
  tiers = setCons IntSet.fromList
