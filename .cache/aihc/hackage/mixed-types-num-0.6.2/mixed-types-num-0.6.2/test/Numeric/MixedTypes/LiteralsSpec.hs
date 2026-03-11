{-|
    Module      :  Numeric.MixedType.LiteralsSpec
    Description :  hspec tests for Literals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.LiteralsSpec (spec) where

import MixedTypesNumPrelude
import qualified Prelude as P

-- import Text.Printf
import Control.Exception (evaluate)

import Test.Hspec
-- import qualified Test.QuickCheck as QC
-- import qualified Test.Hspec.SmallCheck as SC

spec :: Spec
spec = do
  specCanBeInteger tInt
  specCanBeInteger tInteger
  specConversions

specConversions :: Spec
specConversions =
  do
  specConversion tInt tInteger integer int
  it "converting large integer to int throws exception" $ do
    (evaluate $ int (integer (maxBound :: Int) P.+ 1)) `shouldThrow` anyException
  specConversion tInt tRational rational (int . round)
  specConversion tInteger tRational rational round
  specConversion tDouble tRational toRational double
