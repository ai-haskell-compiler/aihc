{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.SupportSpec
-- Description :  Unit tests for ELynx.Tree.SupportSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 21 14:20:09 2020.
module ELynx.Tree.SupportSpec
  ( spec,
  )
where

import ELynx.Tools.InputOutput
import ELynx.Tree
import Test.Hspec

collapseTree :: Tree Phylo Name
collapseTree = parseByteStringWith (oneNewick IqTree) "((a,b)1.0,(c,d)1.0);"

collapseStarTree :: Tree Phylo Name
collapseStarTree = parseByteStringWith (oneNewick Standard) "(a[1.0],b[1.0],c[1.0],d[1.0])[1.0];"

spec :: Spec
spec = do
  describe "collapse" $ do
    it "creates a star tree for 1.0" $ do
      let t = either error id $ toSupportTree collapseTree
          s = either error id $ toSupportTree collapseStarTree
      collapse 0 t `shouldBe` t
      collapse 0.01 t `shouldBe` t
      collapse 0.99 t `shouldBe` t
      collapse 1.0 t `shouldBe` t
      collapse 1.1 t `shouldBe` s
