{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description :  Rate matrix helper functions
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPLv3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  non-portable (not tested)
--
-- Some helper functions that come handy when working with rate matrices of
-- continuous-time discrete-state Markov processes.
--
-- * Changelog
--
-- To be imported qualified.
module ELynx.MarkovProcess.RateMatrix
  ( RateMatrix,
    ExchangeabilityMatrix,
    StationaryDistribution,
    isValid,
    normalizeSD,
    totalRate,
    totalRateWith,
    normalize,
    normalizeWith,
    setDiagonal,
    toExchangeabilityMatrix,
    fromExchangeabilityMatrix,
    getStationaryDistribution,
    exchFromListLower,
    exchFromListUpper,
  )
where

import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra hiding (normalize)
import Numeric.SpecFunctions
import Prelude hiding ((<>))

-- | A rate matrix is just a real matrix.
type RateMatrix = Matrix R

-- | A matrix of exchangeabilities, we have q = e * pi, where q is a rate
-- matrix, e is the exchangeability matrix and pi is the diagonal matrix
-- containing the stationary frequency distribution.
type ExchangeabilityMatrix = Matrix R

-- | Stationary distribution of a rate matrix.
type StationaryDistribution = Vector R

epsRelaxed :: Double
epsRelaxed = 1e-5

-- | True if distribution sums to 1.0.
isValid :: StationaryDistribution -> Bool
isValid d = epsRelaxed > abs (norm_1 d - 1.0)

-- | Normalize a stationary distribution so that the elements sum to 1.0.
normalizeSD :: StationaryDistribution -> StationaryDistribution
normalizeSD d = d / scalar (norm_1 d)

matrixSetDiagToZero :: Matrix R -> Matrix R
matrixSetDiagToZero m = m - diag (takeDiag m)
{-# INLINE matrixSetDiagToZero #-}

-- | Get average number of substitutions per unit time.
totalRateWith :: StationaryDistribution -> RateMatrix -> Double
totalRateWith d m = norm_1 $ d <# matrixSetDiagToZero m

-- | Get average number of substitutions per unit time.
totalRate :: RateMatrix -> Double
totalRate m = totalRateWith (getStationaryDistribution m) m

-- | Normalizes a Markov process generator such that one event happens per unit
-- time. Calculates stationary distribution from rate matrix.
normalize :: RateMatrix -> RateMatrix
normalize m = normalizeWith (getStationaryDistribution m) m

-- | Normalizes a Markov process generator such that one event happens per unit
-- time. Faster, but stationary distribution has to be given.
normalizeWith :: StationaryDistribution -> RateMatrix -> RateMatrix
normalizeWith d m = scale (1.0 / totalRateWith d m) m

-- | Set the diagonal entries of a matrix such that the rows sum to 0.
setDiagonal :: RateMatrix -> RateMatrix
setDiagonal m = diagZeroes - diag (fromList rowSums)
  where
    diagZeroes = matrixSetDiagToZero m
    rowSums = map norm_1 $ toRows diagZeroes

-- | Extract the exchangeability matrix from a rate matrix.
toExchangeabilityMatrix ::
  RateMatrix -> StationaryDistribution -> ExchangeabilityMatrix
toExchangeabilityMatrix m f = m <> diag oneOverF
  where
    oneOverF = cmap (1.0 /) f

-- | Convert exchangeability matrix to rate matrix.
fromExchangeabilityMatrix ::
  ExchangeabilityMatrix -> StationaryDistribution -> RateMatrix
fromExchangeabilityMatrix em d = setDiagonal $ em <> diag d

eps :: Double
eps = 1e-12

normalizeSumVec :: V.Vector Double -> V.Vector Double
normalizeSumVec v = V.map (/ s) v
  where
    s = V.sum v
{-# INLINE normalizeSumVec #-}

-- | Get stationary distribution from 'RateMatrix'. Involves eigendecomposition.
-- If the given matrix does not satisfy the required properties of transition
-- rate matrices and no eigenvector with an eigenvalue nearly equal to 0 is
-- found, an error is thrown. Is there an easier way to calculate the stationary
-- distribution or a better way to handle errors (of course I could use the
-- Maybe monad, but then the error report is just delayed to the calling
-- function)?
getStationaryDistribution :: RateMatrix -> StationaryDistribution
getStationaryDistribution m =
  if eps > abs (magnitude (eVals ! i))
    then normalizeSumVec distReal
    else error "getStationaryDistribution: Could not retrieve stationary distribution."
  where
    (eVals, eVecs) = eig (tr m)
    i = minIndex eVals
    distComplex = toColumns eVecs !! i
    distReal = cmap realPart distComplex

-- The next functions tackle the somewhat trivial, but not easily solvable
-- problem of converting a triangular matrix (excluding the diagonal) given as a
-- list into a symmetric matrix. The diagonal entries are set to zero.

-- Lower triangular matrix. This is how the exchangeabilities are specified in
-- PAML. Conversion from matrix indices (i,j) to list index k.
--
-- (i,j) k
--
-- (0,0) -
-- (1,0) 0  (1,1) -
-- (2,0) 1  (2,1) 2  (2,2) -
-- (3,0) 3  (3,1) 4  (3,2) 5 (3,3) -
-- (4,0) 6  (4,1) 7  (4,2) 8 (4,3) 9 (4,4) -
--   .
--   .
--   .
--
-- k = (i choose 2) + j.
ijToKLower :: Int -> Int -> Int
ijToKLower i j
  | i > j = round (i `choose` 2) + j
  | otherwise = error "ijToKLower: not defined for upper triangular matrix."

-- Upper triangular matrix. Conversion from matrix indices (i,j) to list index
-- k. Matrix is square of size n.
--
-- (i,j) k
--
-- (0,0) -  (0,1) 0  (0,2) 1    (0,3) 2     (0,4) 3     ...
--          (1,1) -  (1,2) n-1  (1,3) n     (1,4) n+1
--                   (2,2) -    (2,3) 2n-3  (2,4) 2n-2
--                              (3,3) -     (3,4) 3n-6
--                                          (4,4) -
--                                                      ...
--
-- k = i*(n-2) - (i choose 2) + (j - 1)
ijToKUpper :: Int -> Int -> Int -> Int
ijToKUpper n i j
  | i < j = i * (n - 2) - round (i `choose` 2) + j - 1
  | otherwise = error "ijToKUpper: not defined for lower triangular matrix."

-- The function is a little weird because HMatrix uses Double indices for Matrix
-- Double builders.
fromListBuilderLower :: (RealFrac a) => [a] -> a -> a -> a
fromListBuilderLower es i j
  | i > j = es !! ijToKLower iI jI
  | i == j = 0.0
  | i < j = es !! ijToKLower jI iI
  | otherwise =
      error
        "Float indices could not be compared during matrix creation."
  where
    iI = round i :: Int
    jI = round j :: Int

-- The function is a little weird because HMatrix uses Double indices for Matrix
-- Double builders.
fromListBuilderUpper :: (RealFrac a) => Int -> [a] -> a -> a -> a
fromListBuilderUpper n es i j
  | i < j = es !! ijToKUpper n iI jI
  | i == j = 0.0
  | i > j = es !! ijToKUpper n jI iI
  | otherwise =
      error
        "Float indices could not be compared during matrix creation."
  where
    iI = round i :: Int
    jI = round j :: Int

checkEs :: (RealFrac a) => Int -> [a] -> [a]
checkEs n es
  | length es == nExp = es
  | otherwise = error eStr
  where
    nExp = round (n `choose` 2)
    eStr =
      unlines
        [ "exchFromListlower: the number of exchangeabilities does not match the matrix size",
          "matrix size: " ++ show n,
          "expected number of exchangeabilities: " ++ show nExp,
          "received number of exchangeabilities: " ++ show (length es)
        ]

-- | Build exchangeability matrix from list denoting lower triangular matrix,
-- and excluding diagonal. This is how the exchangeabilities are specified in
-- PAML.
exchFromListLower :: (RealFrac a, Container Vector a) => Int -> [a] -> Matrix a
exchFromListLower n es = build (n, n) (fromListBuilderLower (checkEs n es))

-- | Build exchangeability matrix from list denoting upper triangular matrix,
-- and excluding diagonal.
exchFromListUpper :: (RealFrac a, Container Vector a) => Int -> [a] -> Matrix a
exchFromListUpper n es = build (n, n) (fromListBuilderUpper n (checkEs n es))
