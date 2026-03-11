{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Moments of probability distributions.
-}
module Numeric.Probability.Moments
    ( Moments (..)
    , fromExpectedPowers
    ) where

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}

-- | The first four commonly used moments of a probability distribution.
data Moments a = Moments
    { mean :: a
    -- ^ [Mean or Expected Value](https://en.wikipedia.org/wiki/Expected_value)
    -- \( \mu \).
    -- Defined as \( \mu = E[X] \).
    , variance :: a
    -- ^ [Variance](https://en.wikipedia.org/wiki/Variance) \( \sigma^2 \).
    -- Defined as \( \sigma^2 = E[(X - \mu)^2] \).
    -- Equal to \( \sigma^2 = E[X^2] - \mu^2 \).
    , skewness :: a
    -- ^ [Skewness](https://en.wikipedia.org/wiki/Skewness) \( \gamma_1 \).
    -- Defined as
    -- \( \gamma_1 = E\left[\left(\frac{(X - \mu)}{\sigma}\right)^3 \right] \).
    , kurtosis :: a
    -- ^ [Kurtosis](https://en.wikipedia.org/wiki/Kurtosis) \( \kappa \).
    -- Defined as
    --  \( \kappa = E\left[\left(\frac{(X - \mu)}{\sigma}\right)^4 \right] \).
    --
    -- The kurtosis is bounded below: \( \kappa \geq \gamma_1^2 + 1 \).
    }
    deriving (Eq, Show, Functor)

-- | Compute the 'Moments' of a probability distribution given
-- the expectation values of the first four powers \( m_k = E[X^k] \).
--
-- > fromExpectedPowers (m1,m2,m3,m4)
fromExpectedPowers
    :: (Ord a, Num a, Fractional a)
    => (a, a, a, a) -> Moments a
fromExpectedPowers (mean, m2, m3, m4)
    | variance == 0 =
        Moments{mean, variance, skewness = 0, kurtosis = 1}
    | otherwise =
        Moments{mean, variance, skewness, kurtosis}
  where
    meanSq = mean * mean

    variance = m2 - meanSq
    sigma = squareRoot variance

    skewness =
        (m3 - 3 * mean * variance - mean * meanSq
        ) / (sigma * variance)

    kurtosis =
        (m4
            - 4 * mean * skewness * sigma * variance
            - 6 * meanSq * variance
            - meanSq * meanSq
        ) / (variance * variance)

-- | Helper function to approximate the square root.
-- Precision: 1e-4 of the given value.
--
-- Uses Heron's iterative method.
squareRoot :: (Ord a, Num a, Fractional a) => a -> a
squareRoot x
    | x < 0 = error "Negative square root input"
    | x == 0 = 0
    | otherwise = goRoot x0
  where
    precision = x / 10000
    x0 = x/2 -- initial guess
    goRoot xi
        | abs (x - xi * xi) <= precision = xi
        | otherwise = goRoot ((xi + x / xi)/2)

{-sqRoot :: a -> a
sqRoot x = 
    let
        y :: Double
        y = toRational x
    in fromRational . toRational . sqrt y
-}
