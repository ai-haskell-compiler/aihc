{-# LANGUAGE GADTs #-}

module StdGenRandomize (randomizeList) where

import Control.Monad.State
import System.Random (StdGen, mkStdGen, randomR)

data RandomState where
  RandomState :: {generator :: StdGen} -> RandomState

-- | Use StdGen to randomize the order of a list of integers

-- | This is used to generate balanced seed data to be randomized with the
--   RealDice raw data

-- | I feel comfortable use StdGen to generate the files, as long as it's not
--   used in the library itself

-- | ==== __Examples__
--   >>> randomizeList [1, 2, 3, 4, 5]
--   [3, 1, 5, 2, 4]
randomizeList :: [Int] -> [Int]
randomizeList xs =
  evalState
    (randomizeListSinglePass xs [])
    (RandomState (mkStdGen 143))

randomizeListSinglePass :: [Int] -> [Int] -> State RandomState [Int]
randomizeListSinglePass [] l' = return l'
randomizeListSinglePass l l' = do
  randomSt <- get
  let (randomIndex, gen') = randomR (0, length l - 1) (generator randomSt)
  let x = l !! randomIndex
  put RandomState {generator = gen'}
  randomizeListSinglePass (filter (/= x) l) (x : l')
