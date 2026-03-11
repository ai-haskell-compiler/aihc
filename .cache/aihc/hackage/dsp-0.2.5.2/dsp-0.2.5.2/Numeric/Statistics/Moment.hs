-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.Moment
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for computing the various moments of a list
--
-- Reference: Ross, NRiC
--
-----------------------------------------------------------------------------

module Numeric.Statistics.Moment (mean, var,
				  stddev, avgdev,
				  skew, kurtosis) where

-- TODO: does mean pass though the list twice?  once to compute the sum,
-- and the second to compute the length?

-- TODO: does var passes through the list twice, once to compute the mean of
-- the squares, and the other to compute the mean?

-- * Functions

-- | Compute the mean of a list
--
-- @Mean(X) = 1\/N sum(i=1..N) x_i @

-- We need to use Prelude.sum intead of sum because of a buglet in the
-- Data.List library that effects nhc98

mean :: (Fractional a) => [a] -> a
mean x = Prelude.sum x / (fromIntegral.length) x

-- | Compute the variance of a list
--
-- @Var(X) = sigma^2@
--
-- @       = 1\/N-1 sum(i=1..N) (x_i-mu)^2 @

-- This is an approximation
-- var x = (mean $ map (^2) x) - mu^2
--    where mu = mean x

var :: (Fractional a) => [a] -> a
var xs = Prelude.sum (map (\x -> (x - mu)^(2::Int)) xs)  / (n - 1)
    where mu = mean xs
	  n = fromIntegral $ length $ xs

-- | Compute the standard deviation of a list
--
-- @ StdDev(X) = sigma = sqrt (Var(X)) @

stddev :: (RealFloat a) => [a] -> a
stddev x = sqrt $ var x

-- | Compute the average deviation of a list
--
-- @ AvgDev(X) = 1\/N sum(i=1..N) |x_i-mu| @

avgdev :: (RealFloat a) => [a] -> a
avgdev xs = Prelude.sum (map (\x -> abs (x - mu)) xs)  / n
    where mu = mean xs
	  n = fromIntegral $ length $ xs

-- | Compute the skew of a list
--
-- @ Skew(X) = 1\/N sum(i=1..N) ((x_i-mu)\/sigma)^3 @

skew :: (RealFloat a) => [a] -> a
skew xs = Prelude.sum (map (\x -> ((x - mu) / sigma)^(3::Int)) xs)  / n
    where mu = mean xs
	  sigma = stddev xs
	  n = fromIntegral $ length $ xs

-- | Compute the kurtosis of a list
--
-- @ Kurt(X) = ( 1\/N sum(i=1..N) ((x_i-mu)\/sigma)^4 ) - 3@

kurtosis :: (RealFloat a) => [a] -> a
kurtosis xs = Prelude.sum (map (\x -> ((x - mu) / sigma)^(4::Int)) xs)  / n - 3
    where mu = mean xs
	  sigma = stddev xs
	  n = fromIntegral $ length $ xs
