{-|
    Module      :  Numeric.MixedType.FieldSpec
    Description :  hspec tests for multiplication and exponentiation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.FieldSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specCanDivNotMixed tInt
  specCanDivNotMixed tInteger
  specCanDivNotMixed tRational
  specCanDiv tInt tInteger
  specCanDiv tRational tInteger
