-- |
-- Module      :  Statistics.Distribution.Dirichlet
-- Description :  Multivariate Dirichlet distribution
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Oct 20 10:10:39 2020.
module Statistics.Distribution.Dirichlet
  ( -- * Dirichlet distribution
    DirichletDistribution (ddGetParameters),
    dirichletDistribution,
    dirichletDensity,
    dirichletSample,

    -- * Symmetric Dirichlet distribution
    DirichletDistributionSymmetric (ddSymGetParameter),
    dirichletDistributionSymmetric,
    dirichletDensitySymmetric,
    dirichletSampleSymmetric,
  )
where

import qualified Data.Vector.Unboxed as V
import Numeric.Log
import Numeric.SpecFunctions
import System.Random.MWC.Distributions
import System.Random.Stateful

-- | The [Dirichlet distribution](https://en.wikipedia.org/wiki/Dirichlet_distribution).
data DirichletDistribution = DirichletDistribution
  { ddGetParameters :: V.Vector Double,
    _getDimension :: Int,
    _getNormConst :: Log Double
  }
  deriving (Eq, Show)

-- Check if vector is strictly positive.
isNegativeOrZero :: V.Vector Double -> Bool
isNegativeOrZero = V.any (<= 0)

-- Inverse multivariate beta function. Does not check if parameters are valid!
invBeta :: V.Vector Double -> Log Double
invBeta v = Exp $ logDenominator - logNominator
  where
    logNominator = V.sum $ V.map logGamma v
    logDenominator = logGamma (V.sum v)

-- | Create a Dirichlet distribution from the given parameter vector.
--
-- Return Left if:
--
-- - The parameter vector has less then two elements.
--
-- - One or more parameters are negative or zero.
dirichletDistribution :: V.Vector Double -> Either String DirichletDistribution
dirichletDistribution v
  | V.length v < 2 =
      Left "dirichletDistribution: Parameter vector is too short."
  | isNegativeOrZero v =
      Left "dirichletDistribution: One or more parameters are negative or zero."
  | otherwise = Right $ DirichletDistribution v (V.length v) (invBeta v)

-- Tolerance.
eps :: Double
eps = 1e-14

-- Check if vector is normalized with tolerance 'eps'.
isNormalized :: V.Vector Double -> Bool
isNormalized v
  | abs (V.sum v - 1.0) > eps = False
  | otherwise = True

-- | Density of the Dirichlet distribution evaluated at a given value vector.
--
-- Return 0 if:
--
-- - The value vector has a different length than the parameter vector.
--
-- - The value vector has elements being negative or zero.
--
-- - The value vector does not sum to 1.0 (with tolerance @eps = 1e-14@).
dirichletDensity :: DirichletDistribution -> V.Vector Double -> Log Double
dirichletDensity (DirichletDistribution as k c) xs
  | k /= V.length xs = 0
  | isNegativeOrZero xs = 0
  | not (isNormalized xs) = 0
  | otherwise = c * Exp logXsPow
  where
    logXsPow = V.sum $ V.zipWith (\a x -> log $ x ** (a - 1.0)) as xs

-- | Sample a value vector from the Dirichlet distribution.
dirichletSample :: StatefulGen g m => DirichletDistribution -> g -> m (V.Vector Double)
dirichletSample (DirichletDistribution as _ _) g = do
  ys <- V.mapM (\a -> gamma a 1.0 g) as
  let s = V.sum ys
  return $ V.map (/ s) ys

-- | See 'DirichletDistribution' but with parameter vector @replicate DIM VAL@.
data DirichletDistributionSymmetric = DirichletDistributionSymmetric
  { ddSymGetParameter :: Double,
    _symGetDimension :: Int,
    _symGetNormConst :: Log Double
  }
  deriving (Eq, Show)

-- Inverse multivariate beta function. Does not check if parameters are valid!
invBetaSym :: Int -> Double -> Log Double
invBetaSym k a = Exp $ logDenominator - logNominator
  where
    logNominator = fromIntegral k * logGamma a
    logDenominator = logGamma (fromIntegral k * a)

-- | Create a symmetric Dirichlet distribution of given dimension and parameter.
--
-- Return Left if:
--
-- - The given dimension is smaller than two.
--
-- - The parameter is negative or zero.
dirichletDistributionSymmetric :: Int -> Double -> Either String DirichletDistributionSymmetric
dirichletDistributionSymmetric k a
  | k < 2 =
      Left "dirichletDistributionSymmetric: The dimension is smaller than two."
  | a <= 0 =
      Left "dirichletDistributionSymmetric: The parameter is negative or zero."
  | otherwise = Right $ DirichletDistributionSymmetric a k (invBetaSym k a)

-- | Density of the symmetric Dirichlet distribution evaluated at a given value
-- vector.
--
-- Return 0 if:
--
-- - The value vector has a different dimension.
--
-- - The value vector has elements being negative or zero.
--
-- - The value vector does not sum to 1.0 (with tolerance @eps = 1e-14@).
dirichletDensitySymmetric :: DirichletDistributionSymmetric -> V.Vector Double -> Log Double
dirichletDensitySymmetric (DirichletDistributionSymmetric a k c) xs
  | k /= V.length xs = 0
  | isNegativeOrZero xs = 0
  | not (isNormalized xs) = 0
  | otherwise = c * Exp logXsPow
  where
    logXsPow = V.sum $ V.map (\x -> log $ x ** (a - 1.0)) xs

-- | Sample a value vector from the symmetric Dirichlet distribution.
dirichletSampleSymmetric ::
  StatefulGen g m =>
  DirichletDistributionSymmetric ->
  g ->
  m (V.Vector Double)
dirichletSampleSymmetric (DirichletDistributionSymmetric a k _) g = do
  ys <- V.replicateM k (gamma a 1.0 g)
  let s = V.sum ys
  return $ V.map (/ s) ys
