{-# LANGUAGE TypeApplications #-}

module Data.BCP47.PathPiecesSpec
  ( spec
  ) where

import Data.BCP47 (BCP47)
import Data.BCP47.PathPieces ()
import Data.BCP47.Roundtrip (roundtrips)
import Test.Hspec
import Test.QuickCheck (property)
import Web.PathPieces (fromPathPiece, toPathPiece)

spec :: Spec
spec = describe "PathPiece" . it "roundtrips" . property $ roundtrips @BCP47
  toPathPiece
  fromPathPiece
