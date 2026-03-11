{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module RealDice.RNGSpec
  ( returns_many_unique_integers,
    returns_many_unique_rolls,
    returns_0_for_inputs_under_1,
    returns_many_unique_floats,
    returns_floats_between_0_and_1,
    returns_many_unique_doubles,
    returns_doubles_between_0_and_1,
  )
where

import Control.Monad.State
import RealDice.DieSpec (returns_0_for_inputs_under_1, returns_many_unique_rolls)
import RealDice.RNG (RDGen, mkRDGen, randomDouble, randomFloat, randomIntR)
import Test.QuickCheck

data RandomState where
  RandomState :: {generator :: RDGen} -> RandomState

generateRandomNumbers :: Int -> [Int]
generateRandomNumbers n =
  evalState (generateRandomNumbersSinglePass 10 []) (RandomState (mkRDGen n))

generateRandomNumbersSinglePass :: Int -> [Int] -> State RandomState [Int]
generateRandomNumbersSinglePass 0 generatedNumbers = return generatedNumbers
generateRandomNumbersSinglePass i generatedNumbers = do
  randomSt <- get
  let (randomInteger, gen') = randomIntR (0, 1000) (generator randomSt)
  put RandomState {generator = gen'}
  generateRandomNumbersSinglePass (i - 1) (randomInteger : generatedNumbers)

returns_many_unique_integers :: Int -> Property
returns_many_unique_integers n =
  n > 0 ==>
    head (generateRandomNumbers n) `notElem` tail (generateRandomNumbers n)

generateRandomFloats :: Int -> [Float]
generateRandomFloats n =
  evalState (generateRandomFloatsSinglePass 10 []) (RandomState (mkRDGen n))

generateRandomFloatsSinglePass :: Int -> [Float] -> State RandomState [Float]
generateRandomFloatsSinglePass 0 generatedNumbers = return generatedNumbers
generateRandomFloatsSinglePass i generatedNumbers = do
  randomSt <- get
  let (randomFloat', gen') = randomFloat 4 (generator randomSt)
  put RandomState {generator = gen'}
  generateRandomFloatsSinglePass (i - 1) (randomFloat' : generatedNumbers)

returns_many_unique_floats :: Int -> Property
returns_many_unique_floats n =
  n > 0 ==>
    head (generateRandomFloats n) `notElem` tail (generateRandomFloats n)

returns_floats_between_0_and_1 :: Int -> Property
returns_floats_between_0_and_1 n =
  n > 0 ==>
    all (\x -> x >= 0 && x <= 1) (generateRandomFloats n)

generateRandomDoubles :: Int -> [Double]
generateRandomDoubles n =
  evalState (generateRandomDoublesSinglePass 10 []) (RandomState (mkRDGen n))

generateRandomDoublesSinglePass :: Int -> [Double] -> State RandomState [Double]
generateRandomDoublesSinglePass 0 generatedDoubles = return generatedDoubles
generateRandomDoublesSinglePass i generatedDoubles = do
  randomSt <- get
  let (randomDouble', gen') = randomDouble 4 (generator randomSt)
  put RandomState {generator = gen'}
  generateRandomDoublesSinglePass (i - 1) (randomDouble' : generatedDoubles)

returns_many_unique_doubles :: Int -> Property
returns_many_unique_doubles n =
  n > 0 ==>
    head (generateRandomDoubles n) `notElem` tail (generateRandomDoubles n)

returns_doubles_between_0_and_1 :: Int -> Property
returns_doubles_between_0_and_1 n =
  n > 0 ==>
    all (\x -> x >= 0 && x <= 1) (generateRandomDoubles n)
