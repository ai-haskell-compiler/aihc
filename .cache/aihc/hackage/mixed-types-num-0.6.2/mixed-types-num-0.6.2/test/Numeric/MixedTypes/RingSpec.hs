{-|
    Module      :  Numeric.MixedType.RingSpec
    Description :  hspec tests for multiplication and exponentiation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.RingSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specCanMulNotMixed tInt
  specCanMulNotMixed tInteger
  specCanMulNotMixed tRational
  specCanMul tInt tInteger tRational
  specCanMul tInteger tRational tInt
  specCanMulSameType tInteger
  specCanMulSameType tRational
  specCanPow tInteger tInteger
  specCanPow tInteger tInt
  specCanPow tRational tInteger
  specCanPow tRational tInt
