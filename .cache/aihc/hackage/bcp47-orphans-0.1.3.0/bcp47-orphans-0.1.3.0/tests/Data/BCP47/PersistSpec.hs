{-# LANGUAGE TypeApplications #-}

module Data.BCP47.PersistSpec
  ( spec
  ) where

import Data.BCP47 (BCP47)
import Data.BCP47.Persist ()
import Data.BCP47.Roundtrip (roundtrips)
import Database.Persist.Class (fromPersistValue, toPersistValue)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "PersistField" . it "roundtrips" . property $ roundtrips @BCP47
  toPersistValue
  fromPersistValue
