{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Serialise.CoreSpec (spec) where

import Common
import Data.Massiv.Array

spec :: Spec
spec = do
  describe "Strategy" $ do
    prop "Comp" $ roundtrip @Comp
  describe "Index" $ do
    prop "Ix2" $ roundtrip @Ix2
    prop "Ix3" $ roundtrip @Ix3
    prop "Ix4" $ roundtrip @Ix4
    prop "Ix5" $ roundtrip @Ix5
  describe "Size" $ do
    prop "Sz1" $ roundtrip @Sz1
    prop "Sz2" $ roundtrip @Sz2
    prop "Sz3" $ roundtrip @Sz3
    prop "Sz4" $ roundtrip @Sz4
    prop "Sz5" $ roundtrip @Sz5
