{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  ELynx.Tree.PhylogenySpec
-- Description :  Unit tests for ELynx.Tree.Phylogeny
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Jul 15 11:05:32 2020.
module ELynx.Tree.PhylogenySpec
  ( spec,
  )
where

import Data.Default
import Data.Either
import qualified Data.Set as S
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (labels)

simpleTree1 :: Tree () String
simpleTree1 = Node () "i" [Node () "j" [Node () "x" [], Node () "y" []], Node () "z" []]

simpleTree2 :: Tree () String
simpleTree2 = Node () "i" [Node () "j" [Node () "y" [], Node () "x" []], Node () "z" []]

simpleTree3 :: Tree () String
simpleTree3 = Node () "i" [Node () "j" [Node () "x" [], Node () "z" []], Node () "y" []]

prop_commutative :: (Eq a, Ord a) => Tree () a -> Tree () a -> Bool
prop_commutative t1 t2 = case (s1, s2) of
  (Left _, Left _) -> True
  (Right x1, Right x2) -> x1 == x2
  _ -> False
  where
    s1 = t1 `equal` t2
    s2 = t2 `equal` t1

simpleSol :: Forest () String
simpleSol =
  [ Node () "i" [Node () "j" [Node () "x" [], Node () "y" []], Node () "z" []],
    Node () "i" [Node () "j" [Node () "z" [], Node () "y" []], Node () "x" []],
    Node () "i" [Node () "j" [Node () "z" [], Node () "x" []], Node () "y" []]
  ]

prop_roots :: Default a => Tree () a -> Bool
prop_roots t@(Node _ _ []) = isLeft $ roots t
prop_roots t@(Node _ _ [_]) = isLeft $ roots t
prop_roots t@(Node _ _ [_, _]) = (length <$> roots t) == Right (length (labels t) - 2)
prop_roots t = (length <$> roots t) == Right (length (labels t) - 1)

prop_roots_total_length :: Default a => Tree Length a -> Bool
prop_roots_total_length t@(Node _ _ [_, _]) =
  all (\x -> abs (totalBranchLength x - l) < 1e-8) $
    either error id $
      roots t
  where
    l = totalBranchLength t
prop_roots_total_length _ = True

spec :: Spec
spec = do
  describe "equal" $ do
    it "correctly handles some test cases" $ do
      simpleTree1 `equal` simpleTree2 `shouldBe` Right True
      simpleTree1 `equal` simpleTree3 `shouldBe` Right False
      simpleTree2 `equal` simpleTree3 `shouldBe` Right False
    it "is commutative" $
      property (prop_commutative :: Tree () Int -> Tree () Int -> Bool)
  describe "roots" $ do
    it "correctly handles leaves and cherries" $ do
      let tleaf = Node () 0 [] :: Tree () Int
          tcherry = Node () 0 [Node () 1 [], Node () 2 []] :: Tree () Int
      roots tleaf `shouldSatisfy` isLeft
      roots tcherry `shouldBe` Right [tcherry]
    it "correctly handles simple trees" $
      either error id (roots simpleTree1) `shouldBe` simpleSol
    modifyMaxSize (* 100) $ do
      it "returns the correct number of rooted trees for arbitrary trees" $
        property (prop_roots :: (Tree () Int -> Bool))
      it "does not change the total tree length" $
        property (prop_roots_total_length :: Tree Length Int -> Bool)
  describe "outgroup" $
    it "correctly handles simple trees" $ do
      let p = fst $ fromBipartition $ either error id $ bipartition simpleTree1
      outgroup p simpleTree1 `shouldBe` Right simpleTree1
      let l = S.singleton "x"
      either error id (outgroup l simpleTree1) `equal` (simpleSol !! 1)
        `shouldBe` Right True
