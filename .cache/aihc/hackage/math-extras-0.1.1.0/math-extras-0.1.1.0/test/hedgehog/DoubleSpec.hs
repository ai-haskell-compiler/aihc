{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module DoubleSpec where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Math.Extras.Double

notFinite :: Gen Double
notFinite = Gen.element [nan, positiveInfinity, negativeInfinity]

prop_fuzzyEq_zeroOrNaNTolerance :: Property
prop_fuzzyEq_zeroOrNaNTolerance = property $ do
  fuzzyEq 0 0.0 (-0.0) === True
  x <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1.0e+10) 1.0e+10), notFinite]
  y <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1.0e+10) 1.0e+10), notFinite]
  fuzzyEq 0 x y === (x == y || (isNaN x && isNaN y))
  fuzzyEq nan x y === fuzzyEq 0 x y

prop_fuzzyEq_finiteTolerance :: Property
prop_fuzzyEq_finiteTolerance = property $ do
  tolerance <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  x <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1000.0) 1000.0), notFinite]
  y <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1000.0) 1000.0), notFinite]
  fuzzyEq tolerance x y === (abs (x - y) <= abs tolerance || x == y || (isNaN x && isNaN y))
  fuzzyEq tolerance x positiveInfinity === (x == positiveInfinity)
  fuzzyEq tolerance x negativeInfinity === (x == negativeInfinity)
  fuzzyEq tolerance positiveInfinity y === (y == positiveInfinity)
  fuzzyEq tolerance negativeInfinity y === (y == negativeInfinity)
  fuzzyEq tolerance nan y === isNaN y
  fuzzyEq tolerance x nan === isNaN x

prop_fuzzyEq_infiniteTolerance :: Property
prop_fuzzyEq_infiniteTolerance = property $ do
  tolerance <- forAll $ Gen.element [positiveInfinity, negativeInfinity]
  x <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1000.0) 1000.0), notFinite]
  y <- forAll $ Gen.choice [Gen.double (Range.linearFrac (-1000.0) 1000.0), notFinite]
  fuzzyEq tolerance x y === ((not (isNaN x) && not (isNaN y)) || (isNaN x && isNaN y))

prop_roundToInteger :: Property
prop_roundToInteger = property $ do
  roundToInteger HalfUp 3.5 === 4
  roundToInteger HalfDown 3.5 === 3
  roundToInteger HalfEven 3.5 === 4
  roundToInteger HalfEven 4.5 === 4
  roundToInteger HalfUp (-3.5) === -4
  roundToInteger HalfDown (-3.5) === -3
  roundToInteger HalfEven (-3.5) === -4
  roundToInteger HalfEven (-4.5) === -4
  positive <- forAll $ Gen.double (Range.linearFrac 0.0 1.0e+10)
  mode <- forAll $ Gen.element [Up, Down, Ceiling, Floor, HalfUp, HalfDown, HalfEven]
  roundToInteger mode positive ===
    if | mode `elem` [Down, Floor] -> floor positive
       | mode `elem` [Up, Ceiling] -> ceiling positive
       | positive - fromIntegral (floor positive :: Integer) == 0.5 ->
           case mode of
             HalfUp -> ceiling positive
             HalfDown -> floor positive
             HalfEven | even (floor positive :: Integer) -> floor positive
                      | otherwise -> ceiling positive
             other -> error (show other)
        | otherwise -> round positive

  negative <- forAll $ Gen.double (Range.linearFrac (-1.0e+10) 0.0)
  roundToInteger mode negative ===
    if | mode `elem` [Down, Ceiling] -> ceiling negative
       | mode `elem` [Up, Floor] -> floor negative
       | negative - fromIntegral (floor negative :: Integer) == 0.5 ->
           case mode of
             HalfUp -> floor negative
             HalfDown -> ceiling negative
             HalfEven | even (floor negative :: Integer) -> floor negative
                      | otherwise -> ceiling negative
             other -> error (show other)
        | otherwise -> round negative

tests :: IO Bool
tests =
  checkParallel $$(discover)
