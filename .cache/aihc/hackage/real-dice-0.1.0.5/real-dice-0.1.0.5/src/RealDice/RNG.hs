{-# LANGUAGE GADTs #-}

-- | This module provides functions to generate random numbers using balanced
--   integer tables randomized by the RealDice data or custom integer tables
module RealDice.RNG
  ( RDGen,
    randomIntR,
    randomFloat,
    randomDouble,
    mkRDGen,
    mkRDGenCustom,
  )
where

import RealDice.Generate.BalancedTables (rdIntsPrime)
import RealDice.Manipulate.GetValueFromRNGTable (getIntByIndex)
import RealDice.Manipulate.RandomizeList (randomizeList)

-- | Stores a balanced table of random integers and an index pointing to the
--   next value to return
data RDGen where
  RDGen :: {index :: Int, rngTable :: [Int]} -> RDGen

-- | Creates a new RDGen with the given index and the default Int table

-- | ==== __Examples__
--   >>> mkRDGen 143
--   {143, rdIntsPrime}
mkRDGen :: Int -> RDGen
mkRDGen i = mkRDGenCustom i rdIntsPrime

-- | Creates a new RDGen with the given index and Int table

-- | Defaults to the RealDice balanced table of random integers if an empty
--   list is given

-- | ==== __Examples__
--   >>> mkRDGenCustom 143 [1, 0, 4, 3, 2]
--   {143, [1, 0, 4, 3, 2]}
--   >>> mkRDGenCustom 143 []
--   {143, rdIntsPrime}
mkRDGenCustom :: Int -> [Int] -> RDGen
mkRDGenCustom i [] = RDGen {index = i, rngTable = rdIntsPrime}
mkRDGenCustom i table = RDGen {index = i, rngTable = table}

-- | Generates a random integer value between minResult and maxResult via a
--   simple table lookup

-- | ==== __Examples__
--   >>> randomIntR (1, 20) (mkRDGen 143)
--   (12, {144, rdIntsPrime})
--   >>> randomIntR (-1000000, 1000000) (mkRDGen 42)
--   (76465, {43, rdIntsPrime})
--   >>> randomIntR (10, 1) (mkRDGen 42)
--   (0, {43, rdIntsPrime})
randomIntR :: (Int, Int) -> RDGen -> (Int, RDGen)
randomIntR (minResult, maxResult) rng
  | minResult > maxResult = (0, rng)
  | otherwise =
      ( getIntByIndex
          (getIntByIndex (index rng) (rngTable rng))
          (randomizeList [minResult .. maxResult]),
        RDGen {index = index rng + 1, rngTable = rngTable rng}
      )

-- Generate a random float value between 0 and 1

-- | ==== __Examples__
--   >>> randomFloat 3 (mkRDGen 143)
--   (0.503, {144, rdIntsPrime})
--   >>> randomFloat 0 (mkRDGen 143)
--   (0, {143, rdIntsPrime})
--   >>> randomFloat (-1) (mkRDGen 143)
--   (0, {143, rdIntsPrime})
randomFloat :: Int -> RDGen -> (Float, RDGen)
randomFloat decimalPrecision rng
  | decimalPrecision < 0 = (0, rng)
  | otherwise = randomFloatSinglePass (decimalPrecision, 0) rng

-- Generates a single digit to be used to compose a random float

-- | ==== __Examples__
--  >>> randomFloatSinglePass (1, 0.003) (mkRDGen 145)
--  (0.503, {146, rdIntsPrime})
--  >>> randomFloatSinglePass (0, 0) (mkRDGen 143)
--  (0, {143, rdIntsPrime})
--  >>> randomFloatSinglePass (-1, 0) (mkRDGen 143)
--  (0, {143, rdIntsPrime})
randomFloatSinglePass :: (Int, Float) -> RDGen -> (Float, RDGen)
randomFloatSinglePass (0, currentFloat) rng = (currentFloat, rng)
randomFloatSinglePass (decimalPlace, currentFloat) rng = do
  let randomDigit = getIntByIndex (getIntByIndex (index rng) (rngTable rng)) (randomizeList [0 .. 9])
  randomFloatSinglePass
    ( decimalPlace - 1,
      currentFloat + (fromIntegral randomDigit / (10 ^ decimalPlace))
    )
    (RDGen {index = index rng + 1, rngTable = rngTable rng})

-- Generates a random float value between 0 and 1

-- | ==== __Examples__
--   >>> randomDouble 3 (mkRDGen 143)
--   (0.503, {144, rdIntsPrime})
--   >>> randomDouble 0 (mkRDGen 143)
--   (0, {143, rdIntsPrime})
--   >>> randomDouble (-1) (mkRDGen 143)
--   (0, {143, rdIntsPrime})
randomDouble :: Int -> RDGen -> (Double, RDGen)
randomDouble decimalPrecision rng
  | decimalPrecision < 0 = (0, rng)
  | otherwise = randomDoubleSinglePass (decimalPrecision, 0) rng

-- Generates a single digit to be used to compose a random float

-- | ==== __Examples__
--   >>> randomDoubleSinglePass (1, 0.003) (mkRDGen 145)
--   (0.503, {146, rdIntsPrime})
--   >>> randomDoubleSinglePass (0, 0) (mkRDGen 143)
--   (0, {143, rdIntsPrime})
--   >>> randomDoubleSinglePass (-1, 0) (mkRDGen 143)
--   (0, {143, rdIntsPrime})
randomDoubleSinglePass :: (Int, Double) -> RDGen -> (Double, RDGen)
randomDoubleSinglePass (0, currentDouble) rng = (currentDouble, rng)
randomDoubleSinglePass (decimalPlace, currentDouble) rng = do
  let randomDigit = getIntByIndex (getIntByIndex (index rng) (rngTable rng)) (randomizeList [0 .. 9])
  randomDoubleSinglePass
    ( decimalPlace - 1,
      currentDouble + (fromIntegral randomDigit / (10 ^ decimalPlace))
    )
    (RDGen {index = index rng + 1, rngTable = rngTable rng})
