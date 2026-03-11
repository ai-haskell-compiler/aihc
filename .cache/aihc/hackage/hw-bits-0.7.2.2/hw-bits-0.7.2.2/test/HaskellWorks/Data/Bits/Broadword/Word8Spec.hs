{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.Data.Bits.Broadword.Word8Spec where

import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Broadword.Word8 as W8
import qualified Hedgehog.Gen                           as G
import qualified Hedgehog.Range                         as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word8" $ do
  it "kBitDiff" $ requireProperty $ do
    a <- forAll $ G.word8 (R.linear 0 0xff)
    b <- forAll $ G.word8 (R.linear 0 0xff)

    W8.kBitDiff 8 a b === a - b
  describe "kBitDiffPos" $ do
    it "subword size 8" $ requireProperty $ do
      a <- forAll $ G.word8 (R.linear 0 0x7f)
      b <- forAll $ G.word8 (R.linear 0 0x7f)

      if a > b
        then W8.kBitDiffPos 8 a b === a - b
        else W8.kBitDiffPos 8 a b === 0
    it "subword size 4" $ requireProperty $ do
      a <- forAll $ G.word8 (R.linear 0 0x7)
      b <- forAll $ G.word8 (R.linear 0 0x7)
      c <- forAll $ G.word8 (R.linear 0 0x7)
      d <- forAll $ G.word8 (R.linear 0 0x7)

      ab <- forAll $ pure (a .|. (b .<. 4))
      cd <- forAll $ pure (c .|. (d .<. 4))

      r  <- forAll $ pure $ W8.kBitDiffPos 4 ab cd

      annotateShow $ "ab = " <> bitShow ab
      annotateShow $ "cd = " <> bitShow cd
      annotateShow $ "r  = " <> bitShow r

      if a > c
        then r .&. 0x0f === a - c
        else r .&. 0x0f === 0

      if b > d
        then (r .>. 4) .&. 0x0f === b - d
        else (r .>. 4) .&. 0x0f === 0

    it "subword size 4 test 1" $ requireProperty $ do
      a <- forAll $ pure (1 :: Word8)
      b <- forAll $ pure (0 :: Word8)
      c <- forAll $ pure (0 :: Word8)
      d <- forAll $ pure (0 :: Word8)

      ab <- forAll $ pure (a .|. (b .<. 4))
      cd <- forAll $ pure (c .|. (d .<. 4))

      r  <- forAll $ pure $ W8.kBitDiffPos 4 ab cd

      annotateShow $ "ab = " <> bitShow ab
      annotateShow $ "cd = " <> bitShow cd
      annotateShow $ "r  = " <> bitShow r

      if a > c
        then r .&. 0x0f === a - c
        else r .&. 0x0f === 0

      if b > d
        then (r .>. 4) .&. 0x0f === b - d
        else (r .>. 4) .&. 0x0f === 0
