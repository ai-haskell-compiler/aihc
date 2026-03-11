-- |
-- Module      :  Statistics.Covariance.GraphicalLasso
-- Description :  Graphical lasso
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Sep 15 09:23:19 2021.
module Statistics.Covariance.GraphicalLasso
  ( graphicalLasso,
  )
where

import Algorithms.GLasso
import Data.Bifunctor
import qualified Numeric.LinearAlgebra as L

-- | Gaussian graphical model based estimator.
--
-- This function estimates both, the covariance and the precision matrices. It
-- is best suited for sparse covariance matrices.
--
-- For now, this is just a wrapper around 'glasso'.
--
-- See Friedman, J., Hastie, T., & Tibshirani, R., Sparse inverse covariance
-- estimation with the graphical lasso, Biostatistics, 9(3), 432–441 (2007).
-- http://dx.doi.org/10.1093/biostatistics/kxm045.
--
-- Return 'Left' if
--
-- - the regularization parameter is out of bounds \([0, \infty)\).
--
-- - only one sample is available.
--
-- - no parameters are available.
--
-- NOTE: This function may call 'error' due to partial library functions.
graphicalLasso ::
  -- | Regularization or lasso parameter; penalty for non-zero covariances. The
  -- higher the lasso parameter, the sparser the estimated inverse covariance
  -- matrix. Must be non-negative.
  Double ->
  -- | Sample data matrix of dimension \(n \times p\), where \(n\) is the number
  -- of samples (rows), and \(p\) is the number of parameters (columns).
  L.Matrix Double ->
  -- | @Either ErrorString (Covariance matrix, Precision matrix)@.
  Either String (L.Herm Double, L.Herm Double)
graphicalLasso l xs
  | l < 0 = Left "graphicalLasso: Regularization parameter is negative."
  | n < 2 = Left "graphicalLasso: Need more than one sample."
  | p < 1 = Left "graphicalLasso: Need at least one parameter."
  | otherwise =
      Right $
        bimap convert convert $ glasso p (L.flatten $ L.unSym sigma) l
  where
    n = L.rows xs
    p = L.cols xs
    (_, sigma) = L.meanCov xs
    convert = L.trustSym . L.reshape p
