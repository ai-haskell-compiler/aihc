{-|
    Module      :  Numeric.MixedType.AddSubSpec
    Description :  hspec tests for addition and subtraction
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.AddSubSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specCanAddNotMixed tInt
  specCanAddNotMixed tInteger
  specCanAddNotMixed tRational
  -- specCanAddNotMixed (tComplex tRational)
  specCanAdd tInt tInteger tRational
  specCanAdd tInteger tRational tInt
  -- specCanAdd tInteger tRational (tComplex tRational)
  specCanAddSameType tInteger
  specCanAddSameType tRational
  -- specCanAddSameType (tComplex tRational)
  specCanSubNotMixed tInt
  specCanSubNotMixed tInteger
  specCanSubNotMixed tRational
  -- specCanSubNotMixed (tComplex tRational)
  specCanSub tInt tInteger
  specCanSub tInt tRational
  specCanSub tInteger tRational
  -- specCanSub tInteger (tComplex tRational)
