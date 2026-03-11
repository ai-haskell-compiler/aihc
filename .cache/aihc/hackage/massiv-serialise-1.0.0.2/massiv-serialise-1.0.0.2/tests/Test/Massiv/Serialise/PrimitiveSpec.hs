{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Serialise.PrimitiveSpec (spec) where

import Common
import Data.Massiv.Array
import Data.Word

spec :: Spec
spec = do
  describe "P" $ do
    roundtripArraysSpec @P @Word32
    roundtripArraysSpec @P @Word64
    roundtripArraysSpec @P @Word
