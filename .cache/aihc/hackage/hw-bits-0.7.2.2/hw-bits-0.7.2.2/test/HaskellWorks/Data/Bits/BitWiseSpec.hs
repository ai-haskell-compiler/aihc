{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitWiseSpec (spec) where

import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Bit             as Bit
import qualified Data.Bits            as B
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.BitWiseSpec" $ do
  describe "for popCount0" $ do
    it "for Word8 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      popCount0 w === bitLength w - fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      popCount0 w === bitLength w - fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word32 R.constantBounded
      popCount0 w === bitLength w - fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      popCount0 w === bitLength w - fromIntegral (B.popCount w)
    it "for [Word8] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word8 R.constantBounded)
      popCount0 w === bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word16] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word16 R.constantBounded)
      popCount0 w === bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word32] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word32 R.constantBounded)
      popCount0 w === bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word64] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word64 R.constantBounded)
      popCount0 w === bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word8] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word8 R.constantBounded)
      popCount0 w === popCount0 (DVS.fromList w)
    it "for [Word16] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word16 R.constantBounded)
      popCount0 w === popCount0 (DVS.fromList w)
    it "for [Word32] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word32 R.constantBounded)
      popCount0 w === popCount0 (DVS.fromList w)
    it "for [Word64] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word64 R.constantBounded)
      popCount0 w === popCount0 (DVS.fromList w)
    it "for [Bool] matches DVU Bit.Bit" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 1000) G.bool
      popCount0 w === popCount0 (DVU.fromList (fmap Bit.Bit w))
  describe "for popCount1" $ do
    it "for Word8 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      popCount1 w === fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      popCount1 w === fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word32 R.constantBounded
      popCount1 w === fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      popCount1 w === fromIntegral (B.popCount w)
    it "for [Word8] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word8 R.constantBounded)
      popCount1 w === sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word16] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word16 R.constantBounded)
      popCount1 w === sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word32] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word32 R.constantBounded)
      popCount1 w === sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word64] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word64 R.constantBounded)
      popCount1 w === sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word8] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word8 R.constantBounded)
      popCount1 w === popCount1 (DVS.fromList w)
    it "for [Word16] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word16 R.constantBounded)
      popCount1 w === popCount1 (DVS.fromList w)
    it "for [Word32] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word32 R.constantBounded)
      popCount1 w === popCount1 (DVS.fromList w)
    it "for [Word64] matches Data.Bits implementation" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 10) (G.word64 R.constantBounded)
      popCount1 w === popCount1 (DVS.fromList w)
    it "for [Bool] matches DVU Bit.Bit" $ requireProperty $ do
      w <- forAll $ G.list (R.constant 0 1000) G.bool
      popCount1 w === popCount1 (DVU.fromList (fmap Bit.Bit w))
