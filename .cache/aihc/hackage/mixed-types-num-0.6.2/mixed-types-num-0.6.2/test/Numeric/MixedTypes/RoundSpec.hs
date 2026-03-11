{-|
    Module      :  Numeric.MixedType.RoundSpec
    Description :  hspec tests for round, etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.RoundSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specCanDivIMod tCNInteger
  specCanDivIMod tCNRational
  specCanRound tRational
  -- specCanRound tDouble
