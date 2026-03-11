{-# LANGUAGE TypeApplications #-}

module Data.BCP47.SerialiseSpec
  ( spec
  ) where

import Codec.Serialise
import Data.BCP47 (BCP47)
import Data.BCP47.Serialise ()
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "Serialise" . it "roundtrips" . property $ \x ->
  deserialise @BCP47 (serialise @BCP47 x) `shouldBe` x
