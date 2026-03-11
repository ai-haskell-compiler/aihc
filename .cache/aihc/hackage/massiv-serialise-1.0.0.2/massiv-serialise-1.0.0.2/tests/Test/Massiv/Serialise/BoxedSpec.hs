{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Serialise.BoxedSpec (spec) where

import Common
import Data.Massiv.Array

spec :: Spec
spec = do
  describe "B" $ do
    roundtripArraysSpec @B @Integer
  describe "BN" $ do
    roundtripArraysSpec @BN @Integer
  describe "BL" $ do
    roundtripArraysSpec @BL @Integer
