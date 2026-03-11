{-# LANGUAGE TypeApplications #-}

module Data.BCP47.AesonSpec
  ( spec
  ) where

import Data.Aeson
import Data.BCP47
import Data.BCP47.Aeson ()
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ToJSON/FromJSON" $ do
    prop "roundtrips" $ \x ->
      decode (encode @BCP47 x) `shouldBe` Just x
