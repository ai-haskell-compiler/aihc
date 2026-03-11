-- |
-- Module      :  Statistics.Covariance.OracleApproximatingShrinkage
-- Description :  Iterative shrinkage based covariance estimator
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Sep 10 13:58:15 2021.
module Statistics.Covariance.OracleApproximatingShrinkage
  ( oracleApproximatingShrinkage,
  )
where

import qualified Numeric.LinearAlgebra as L
import Statistics.Covariance.Internal.Tools

-- | Iterative shrinkage based covariance estimator.
--
-- See Chen, Y., Wiesel, A., Eldar, Y. C., & Hero, A. O., Shrinkage algorithms
-- for mmse covariance estimation, IEEE Transactions on Signal Processing,
-- 58(10), 5016–5029 (2010). http://dx.doi.org/10.1109/tsp.2010.2053029.
--
-- Return 'Left' if
--
-- - only one sample is available.
--
-- - no parameters are available.
--
-- NOTE: This function may call 'error' due to partial library functions.
oracleApproximatingShrinkage ::
  -- | Sample data matrix of dimension \(n \times p\), where \(n\) is the number
  -- of samples (rows), and \(p\) is the number of parameters (columns).
  L.Matrix Double ->
  Either String (L.Herm Double)
oracleApproximatingShrinkage xs
  | n < 2 = Left "oracleApproximatingShrinkage: Need more than one sample."
  | p < 1 = Left "oracleApproximatingShrinkage: Need at least one parameter."
  | otherwise = Right $ shrinkWith rho sigma mu im
  where
    n = L.rows xs
    p = L.cols xs
    (_, sigma) = L.meanCov xs
    im = L.trustSym $ L.ident p
    -- Trace and squared trace of sigma.
    trS = trace $ L.unSym sigma
    tr2S = trS * trS
    -- Trace of (sigma squared).
    s2 = let s = L.unSym sigma in s L.<> s
    trS2 = trace s2
    -- NOTE: Equation 25 specifying phi is erroneous.
    -- Shrinkage factor (Equation 23).
    n' = fromIntegral n
    p' = fromIntegral p
    rhoNominator = tr2S - recip p' * trS2
    rhoDenominator = (n' - 1 / p') * (trS2 - recip p' * tr2S)
    rho' = rhoNominator / rhoDenominator
    rho = min rho' 1.0
    -- Scaling factor of the identity matrix (Equation 3).
    mu = trS / p'
