{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Excess.PartialMinMaxExcess0Spec (spec) where

import HaskellWorks.Data.Excess.MinMaxExcess0
import HaskellWorks.Data.Excess.PartialMinMaxExcess0
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Excess.PartialMinMaxExcess0Spec" $ do
  describe "Equivalent to partialMinMaxExcess1 implementation" $ do
    it "For word8" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 8 w
    it "For word16" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 16 w
    it "For word32" $ requireProperty $ do
      w <- forAll $ G.word32 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 32 w
    it "For word64" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 64 w
