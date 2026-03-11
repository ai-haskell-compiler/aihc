{-|
    Module      :  Numeric.MixedType.EqOrdSpec
    Description :  hspec tests for comparison operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.EqOrdSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specHasEqNotMixed tInt
  specHasEqNotMixed tInteger
  specHasEqNotMixed tRational
  specHasEqNotMixed tDouble
  specHasEq tInt tInteger tRational
  specHasEq tInteger tRational tInt
  specHasEq tInteger tDouble tInt
  specCanTestZero tInt
  specCanTestZero tInteger
  specCanTestZero tRational
  specCanPickNonZero tInt
  specCanPickNonZero tInteger
  specCanPickNonZero tRational
  specHasOrderNotMixed tInt
  specHasOrderNotMixed tInteger
  specHasOrderNotMixed tRational
  specHasOrderNotMixed tDouble
  specHasOrder tInt tInteger tRational
  specHasOrder tInteger tRational tInt
  specHasOrder tInteger tDouble tInt
