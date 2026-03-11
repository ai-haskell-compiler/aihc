{-# LANGUAGE TypeApplications #-}

module Data.BCP47.CsvSpec
  ( spec
  ) where

import Data.BCP47 (BCP47)
import Data.BCP47.Csv ()
import Data.BCP47.Roundtrip (roundtrips)
import Data.Csv (parseField, runParser, toField)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec =
  describe "ToField/FromField"
    . it "roundtrips"
    . property
    . roundtrips @BCP47 toField
    $ runParser
    . parseField
