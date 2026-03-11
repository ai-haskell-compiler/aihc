{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Log2Spec (spec) where

import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Log2Spec" $ do
  it "Log2 Word64" . requireProperty $ do
    w <- forAll $ G.word64 (R.constant 1 maxBound)
    log2 w === floor (log (fromIntegral w) / log 2 :: Double)
  it "Log2 Word32" . requireProperty $ do
    w <- forAll $ G.word64 (R.constant 1 maxBound)
    log2 w === floor (log (fromIntegral w) / log 2 :: Double)
