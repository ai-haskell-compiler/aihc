{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module RealDice.DieSpec
  ( returns_many_unique_rolls,
    returns_0_for_inputs_under_1,
  )
where

import Control.Monad.State
import RealDice.Die (DieGen, mkDieGen, roll1d)
import Test.QuickCheck

data DieSt where
  DieSt :: {generator :: DieGen} -> DieSt

generateRolls :: Int -> [Int]
generateRolls n =
  evalState (generateRollsSinglePass 10 []) (DieSt (mkDieGen n))

generateRollsSinglePass :: Int -> [Int] -> State DieSt [Int]
generateRollsSinglePass 0 generatedRolls = return generatedRolls
generateRollsSinglePass i generatedRolls = do
  randomSt <- get
  let (randomInteger, gen') = roll1d 1000 (generator randomSt)
  put DieSt {generator = gen'}
  generateRollsSinglePass (i - 1) (randomInteger : generatedRolls)

returns_many_unique_rolls :: Int -> Property
returns_many_unique_rolls n =
  n
    > 0
    ==> head (generateRolls n)
      `notElem` tail (generateRolls n)

returns_0_for_inputs_under_1 :: Int -> Property
returns_0_for_inputs_under_1 n = n < 1 ==> head (generateRolls n) == 0
