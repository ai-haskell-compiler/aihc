-- |
-- Module      :  Statistics.Covariance
-- Description :  Estimate covariance matrices from sample data
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Sep 14 13:02:15 2021.
module Statistics.Covariance
  ( -- * Empirical estimator
    empiricalCovariance,

    -- * Shrinkage based estimators

    -- | See the overview on shrinkage estimators provided by
    -- [scikit-learn](https://scikit-learn.org/dev/modules/covariance.html#shrunk-covariance).
    module Statistics.Covariance.LedoitWolf,
    module Statistics.Covariance.RaoBlackwellLedoitWolf,
    module Statistics.Covariance.OracleApproximatingShrinkage,

    -- * Gaussian graphical model based estimators
    module Statistics.Covariance.GraphicalLasso,

    -- * Misc
    DoCenter (..),

    -- * Helper functions
    scale,
    rescaleSWith,
    rescalePWith,
  )
where

import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.Devel as L
import Statistics.Covariance.GraphicalLasso
import Statistics.Covariance.LedoitWolf
import Statistics.Covariance.OracleApproximatingShrinkage
import Statistics.Covariance.RaoBlackwellLedoitWolf
import Statistics.Covariance.Types
import qualified Statistics.Sample as S

-- | Empirical or sample covariance.
--
-- Classical maximum-likelihood estimator; asymptotically unbiased but sensitive
-- to outliers.
--
-- Re-export of the empirical covariance 'L.meanCov' provided by
-- [hmatrix](https://hackage.haskell.org/package/hmatrix).
--
-- NOTE: This function may call 'error'.
empiricalCovariance ::
  -- | Data matrix of dimension NxP, where N is the number of observations, and
  -- P is the number of parameters.
  L.Matrix Double ->
  L.Herm Double
empiricalCovariance = snd . L.meanCov

scaleWith ::
  -- Vector of means (length P).
  L.Vector Double ->
  -- Vector of standard deviations (length P)
  L.Vector Double ->
  -- Data matrix of dimension N x P.
  L.Matrix Double ->
  -- Data matrix with means 0 and variance 1.0.
  L.Matrix Double
scaleWith ms ss = L.mapMatrixWithIndex (\(_, j) x -> (x - ms VS.! j) / (ss VS.! j))

-- | Center and scales columns.
--
-- Normalize a data matrix to have means 0 and standard deviations/variances
-- 1.0. The estimated covariance matrix of a scaled data matrix is a correlation
-- matrix, which is easier to estimate.
scale ::
  -- | Sample data matrix of dimension \(n \times p\), where \(n\) is the number
  -- of samples (rows), and \(p\) is the number of parameters (columns).
  L.Matrix Double ->
  -- | (Means, Standard deviations, Centered and scaled matrix)
  (L.Vector Double, L.Vector Double, L.Matrix Double)
scale xs = (ms, ss, scaleWith ms ss xs)
  where
    msVs = map S.meanVariance $ L.toColumns xs
    ms = L.fromList $ map fst msVs
    ss = L.fromList $ map (sqrt . snd) msVs

-- | Convert a correlation matrix with given standard deviations to original
-- scale.
rescaleSWith ::
  -- | Vector of standard deviations of length \(p\).
  L.Vector Double ->
  -- | Normalized correlation matrix of dimension \(p \times p\).
  L.Matrix Double ->
  -- | Covariance matrix of original scale and of dimension \(p \times p\).
  L.Matrix Double
rescaleSWith ss = L.mapMatrixWithIndex (\(i, j) x -> x * (ss VS.! i) * (ss VS.! j))

-- | See 'rescaleSWith' but for precision matrices.
rescalePWith ::
  L.Vector Double ->
  L.Matrix Double ->
  L.Matrix Double
rescalePWith ss = rescaleSWith (VS.map recip ss)
