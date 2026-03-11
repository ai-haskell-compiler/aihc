{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Broadword.Word64Spec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Broadword.Word64 as W8
import qualified Hedgehog.Gen                            as G
import qualified Hedgehog.Range                          as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word64" $ do
  it "kBitDiff" $ requireProperty $ do
    a <- forAll $ G.word64 (R.linear 0 0xffffffffffffffff)
    b <- forAll $ G.word64 (R.linear 0 0xffffffffffffffff)

    W8.kBitDiff 64 a b === a - b
  it "kBitDiffPos" $ requireProperty $ do
    a <- forAll $ G.word64 (R.linear 0 0x7fffffffffffffff)
    b <- forAll $ G.word64 (R.linear 0 0x7fffffffffffffff)

    if a > b
      then W8.kBitDiffPos 64 a b === a - b
      else W8.kBitDiffPos 64 a b === 0
