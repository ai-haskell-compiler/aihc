{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Serialise.StorableSpec (spec) where

import Common
import Data.Massiv.Array
import Data.Word

spec :: Spec
spec =
  describe "S" $ do
    roundtripArraysSpec @S @Bool
    roundtripArraysSpec @S @Word32
    roundtripArraysSpec @S @Word64
    roundtripArraysSpec @S @Word
