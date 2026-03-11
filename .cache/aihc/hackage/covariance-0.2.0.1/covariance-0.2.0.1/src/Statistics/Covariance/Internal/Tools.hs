-- |
-- Module      :  Statistics.Covariance.Internal.Tools
-- Description :  Common functions
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Sep 10 09:47:00 2021.
module Statistics.Covariance.Internal.Tools
  ( centerWith,
    shrinkWith,
    trace,
  )
where

import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.Devel as L

centerWith ::
  -- Mean vector of dimension P.
  L.Vector Double ->
  -- Data matrix of dimension N x P.
  L.Matrix Double ->
  -- Data matrix with means 0.
  L.Matrix Double
centerWith ms = L.mapMatrixWithIndex (\(_, j) x -> x - ms VS.! j)

-- Shrinkage a covariance matrix.
shrinkWith ::
  -- Shrinkage factor.
  Double ->
  -- Sample covariance matrix.
  L.Herm Double ->
  -- Scale of identity matrix (trace of sample covariance matrix divided by
  -- dimension). See Chen2010b, Equation 3.
  Double ->
  -- Identity matrix.
  L.Herm Double ->
  L.Herm Double
shrinkWith rho sigma mu im
  | rho < 0.0 = error "shrinkWith: Bug! Shrinkage factor is negative."
  | rho > 1.0 = error "shrinkWith: Bug! Shrinkage factor is larger than 1.0."
  | mu < 0.0 = error "shrinkWith: Bug! Scaling factor of identity matrix is negative."
  | rho == 1.0 = L.trustSym $ L.scale mu (L.unSym im)
  | otherwise =
      L.trustSym $
        L.scale (1.0 - rho) (L.unSym sigma)
          + L.scale (rho * mu) (L.unSym im)

-- Trace of a matrix.
trace :: L.Matrix Double -> Double
trace = L.sumElements . L.takeDiag
