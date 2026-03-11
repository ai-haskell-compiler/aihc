{-|
    Module      :  Numeric.MixedType.MinMaxAbsSpec
    Description :  hspec tests for min, max, negation and abs
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.MinMaxAbsSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specCanMinMaxNotMixed tInt
  specCanMinMaxNotMixed tInteger
  specCanMinMaxNotMixed tRational
  specCanMinMax tInt tInteger tRational
  specCanMinMax tInteger tRational tInt
  specCanNegNum tInt
  specCanNegNum tInteger
  specCanNegNum tRational
  specCanAbs tInt
  specCanAbs tInteger
  specCanAbs tRational
