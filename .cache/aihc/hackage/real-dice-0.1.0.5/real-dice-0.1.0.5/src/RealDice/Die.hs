{-# LANGUAGE GADTs #-}

-- | This module provides the DieGen data type and functions for using Dice to
--   generate random positive integers via balanced tables randomized by the
--   RealDice data or custom integer tables
module RealDice.Die (DieGen, roll1d, mkDieGen, mkDieGenCustom) where

import RealDice.Generate.BalancedTables (rdIntsPrime)
import RealDice.Manipulate.GetValueFromRNGTable (getIntByIndex)
import RealDice.Manipulate.RandomizeList (randomizeList)

-- | Stores a balanced table of random integers and an index pointing at the
--   next value to return
data DieGen where
  DieGen :: {index :: Int, intTable :: [Int]} -> DieGen

-- | Creates a new DieGen with the given index and the default Int table

-- | ==== __Examples__
--   >>> mkDieGen 143
--   {143, rdIntsPrime}
mkDieGen :: Int -> DieGen
mkDieGen i = mkDieGenCustom i rdIntsPrime

-- | Creates a new DieGen with the given index and Int table

-- | Defaults to the RealDice balanced table of random integers if an empty
--   list is given

-- | ==== __Examples__
--   >>> mkDieGenCustom 143 [1, 0, 4, 3, 2]
--   {143, [1, 0, 4, 3, 2]}
--   >>> mkDieGenCustom 143 []
--   {143, rdIntsPrime}
mkDieGenCustom :: Int -> [Int] -> DieGen
mkDieGenCustom i [] = DieGen {index = i, intTable = rdIntsPrime}
mkDieGenCustom i table = DieGen {index = i, intTable = table}

-- | Generates a random integer value between 1 and n via a simple table lookup

-- | ==== __Examples__
--   >>> roll1d 20 (mkDieGen 143)
--   (12, {144, rdIntsPrime})
roll1d ::
  -- | The number of sides on the die
  Int ->
  -- | The DieGen to use
  DieGen ->
  --  | The updated index
  ( Int,
    --  | The updated DieGen
    DieGen
  )
roll1d n die
  | n < 1 = (0, die)
  | otherwise =
      ( getIntByIndex
          (getIntByIndex (index die) (intTable die))
          (randomizeList [1 .. n]),
        DieGen {index = index die + 1, intTable = intTable die}
      )
