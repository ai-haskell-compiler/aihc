{-# LANGUAGE GADTs #-}

-- | Module for randomizing the order of elements in a list.
--   Used to generate randomized lists for table-based random generation
module RealDice.Manipulate.RandomizeList
  ( randomizeList,
    randomizeWithCustomBools,
  )
where

import Control.Monad.State
import RealDice.Generate.BalancedTables (rdBoolsPrime)
import RealDice.Manipulate.GetValueFromRNGTable (getBoolByIndex)

-- | Stores the index of the next value to return from a randomized list
data RandomState where
  RandomState :: {index :: Int} -> RandomState

-- | Randomizes the order of a list of integers using the default balanced
--   table of RealDice randomized booleans

-- | ==== __Examples__
--   >>> randomizeList [1, 2, 3, 4, 5]
--   [5,3,1,2,4]
randomizeList :: [Int] -> [Int]
randomizeList xs = randomizeWithCustomBools xs rdBoolsPrime

-- | Randomizes the order of a list of integers using a custom list of booleans

-- | ==== __Examples__
--   >>> randomizeWithCustomBools [1, 2, 3, 4, 5] [True, False, False, True, True]
--   [5,4,1,2,3]
randomizeWithCustomBools :: [Int] -> [Bool] -> [Int]
randomizeWithCustomBools xs boolList =
  evalState
    (randomizeListWithCustomBoolListSinglePass xs [] boolList)
    (RandomState 0)

randomizeListWithCustomBoolListSinglePass ::
  [Int] -> [Int] -> [Bool] -> State RandomState [Int]
randomizeListWithCustomBoolListSinglePass [] l' _ = return l'
randomizeListWithCustomBoolListSinglePass l l' boolList = do
  random <- get
  put RandomState {index = index random + 1}
  if getBoolByIndex (index random) boolList
    then
      randomizeListWithCustomBoolListSinglePass
        (drop 1 l)
        (head l : l')
        boolList
    else
      randomizeListWithCustomBoolListSinglePass
        (drop 1 l)
        (l' ++ [head l])
        boolList
