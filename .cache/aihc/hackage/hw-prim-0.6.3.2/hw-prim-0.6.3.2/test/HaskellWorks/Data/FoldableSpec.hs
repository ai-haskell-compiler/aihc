{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.FoldableSpec (spec) where

import Data.Maybe
import HaskellWorks.Data.Foldable
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Data.FoldableSpec" $ do
  it "foldFirst" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 0 10) (G.int R.constantBounded)
    foldFirst as === listToMaybe as
  it "foldLast" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 0 10) (G.int R.constantBounded)
    foldLast as === listToMaybe (reverse as)
