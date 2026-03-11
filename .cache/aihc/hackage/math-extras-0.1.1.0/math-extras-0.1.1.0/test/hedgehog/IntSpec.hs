{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module IntSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Math.Extras.Int

prop_saturatedMultiply :: Property
prop_saturatedMultiply = property $ do
  x <- forAll $ Gen.int (Range.linear minBound maxBound)
  y <- forAll $ Gen.int (Range.linear minBound maxBound)
  let z = toInteger x * toInteger y
  saturatedMultiply x y === if | z > toInteger (maxBound :: Int) -> maxBound
                               | z < toInteger (minBound :: Int) -> minBound
                               | otherwise -> fromInteger z

prop_saturatedPow :: Property
prop_saturatedPow = property $ do
  x <- forAll $ Gen.int (Range.linear minBound maxBound)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let z = toInteger x ^ toInteger y
  saturatedPow x y === if | z > toInteger (maxBound :: Int) -> maxBound
                          | z < toInteger (minBound :: Int) -> minBound
                          | otherwise -> fromInteger z

tests :: IO Bool
tests =
  checkParallel $$(discover)
