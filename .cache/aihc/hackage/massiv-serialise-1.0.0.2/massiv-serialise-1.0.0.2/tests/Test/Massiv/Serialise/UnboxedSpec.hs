{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Serialise.UnboxedSpec (spec) where

import Common
import Data.Massiv.Array

spec :: Spec
spec = do
  describe "U" $ do
    roundtripArraysSpec @U @Bool
    roundtripArraysSpec @U @(Int, Word)
