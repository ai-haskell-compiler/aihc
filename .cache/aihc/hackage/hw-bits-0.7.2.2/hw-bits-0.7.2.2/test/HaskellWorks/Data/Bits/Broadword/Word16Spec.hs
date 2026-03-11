{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Broadword.Word16Spec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Broadword.Word16 as W8
import qualified Hedgehog.Gen                            as G
import qualified Hedgehog.Range                          as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word16" $ do
  it "kBitDiff" $ requireProperty $ do
    a <- forAll $ G.word16 (R.linear 0 0xffff)
    b <- forAll $ G.word16 (R.linear 0 0xffff)

    W8.kBitDiff 16 a b === a - b
  it "kBitDiffPos" $ requireProperty $ do
    a <- forAll $ G.word16 (R.linear 0 0x7fff)
    b <- forAll $ G.word16 (R.linear 0 0x7fff)

    if a > b
      then W8.kBitDiffPos 16 a b === a - b
      else W8.kBitDiffPos 16 a b === 0
