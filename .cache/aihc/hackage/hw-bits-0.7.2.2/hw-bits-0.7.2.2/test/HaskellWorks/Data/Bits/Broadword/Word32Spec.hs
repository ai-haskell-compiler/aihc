{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Broadword.Word32Spec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Broadword.Word32 as W8
import qualified Hedgehog.Gen                            as G
import qualified Hedgehog.Range                          as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word32" $ do
  it "kBitDiff" $ requireProperty $ do
    a <- forAll $ G.word32 (R.linear 0 0xffffffff)
    b <- forAll $ G.word32 (R.linear 0 0xffffffff)

    W8.kBitDiff 32 a b === a - b
  it "kBitDiffPos" $ requireProperty $ do
    a <- forAll $ G.word32 (R.linear 0 0x7fffffff)
    b <- forAll $ G.word32 (R.linear 0 0x7fffffff)

    if a > b
      then W8.kBitDiffPos 32 a b === a - b
      else W8.kBitDiffPos 32 a b === 0
