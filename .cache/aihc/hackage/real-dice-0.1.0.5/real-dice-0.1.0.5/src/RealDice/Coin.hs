{-# LANGUAGE GADTs #-}

-- | This module exports the CoinGen data type and functions for flipping Coins
--   to generate random boolean values via balanced tables randomized by the
--   RealDice data or custom boolean tables
module RealDice.Coin (CoinGen, flipCoin, mkCoinGen, mkCoinGenCustom) where

import RealDice.Generate.BalancedTables (rdBoolsPrime)
import RealDice.Manipulate.GetValueFromRNGTable (getBoolByIndex)

-- | Stores a balanced table of random boolean values and an index pointing to
--   the next value to return
data CoinGen where
  CoinGen :: {index :: Int, boolTable :: [Bool]} -> CoinGen

-- | Creates a new CoinGen with the given index and the default bool table

-- | ==== __Examples__
--   >>> mkCoinGen 143
--   {143, rdBoolsPrime}
mkCoinGen :: Int -> CoinGen
mkCoinGen i = mkCoinGenCustom i rdBoolsPrime

-- | Creates a new CoinGen with the given index and bool table

-- | Defaults to the RealDice balanced table of random booleans if an empty
--   list is given

-- | ==== __Examples__
--   >>> mkCoinGenCustom 143 [True, False, False, True, True]
--   {143, [True, False, False, True, True]}
--   >>> mkCoinGenCustom 143 []
--   {143, rdBoolsPrime}
mkCoinGenCustom :: Int -> [Bool] -> CoinGen
mkCoinGenCustom i [] = CoinGen {index = i, boolTable = rdBoolsPrime}
mkCoinGenCustom i table = CoinGen {index = i, boolTable = table}

-- | Generates a random boolean value via a simple table lookup

-- | ==== __Examples__
--   >>> flipCoin (mkDieGen 143)
--   (False, {144, rdBoolsPrime})
flipCoin :: CoinGen -> (Bool, CoinGen)
flipCoin coin =
  ( getBoolByIndex (index coin) (boolTable coin),
    CoinGen {index = index coin + 1, boolTable = boolTable coin}
  )
