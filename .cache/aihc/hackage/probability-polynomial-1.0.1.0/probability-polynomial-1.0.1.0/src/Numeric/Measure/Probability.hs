{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Probability measures on the number line.
-}
module Numeric.Measure.Probability
    ( -- * Type
      Prob
    , dirac
    , uniform
    , distribution
    , fromDistribution
    , measure
    , fromMeasure
    , unsafeFromMeasure

      -- * Observations
    , support
    , expectation
    , moments

      -- * Operations, numerical
    , choice
    , translate
    , convolve
    ) where

import Control.DeepSeq
    ( NFData
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Measure.Finite.Mixed
    ( Measure
    )
import Numeric.Polynomial.Simple
    ( Poly
    )
import Numeric.Probability.Moments
    ( Moments (..)
    , fromExpectedPowers
    )

import qualified Numeric.Measure.Finite.Mixed as M
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A
-- [probability measure](https://en.wikipedia.org/wiki/Probability_measure)
-- on the number line.
--
-- A probability measure is a 'M.Measure' whose 'M.total' is @1@.
newtype Prob a = Prob (Measure a)
    -- INVARIANT: 'M.isPositive' equals 'True'.
    -- INVARIANT: 'M.total' equals 1
    deriving (Show, NFData)

-- | View the probability measure as a 'M.Measure'.
measure :: (Ord a, Num a) => Prob a -> Measure a
measure (Prob m) = m

-- | View a 'M.Measure' as a probability distribution.
--
-- The measure @m@ must be positive, with total weight @1@, that is
--
-- > isPositive m == True
-- > total m == 1
--
-- These preconditions are checked and the function returns 'Nothing'
-- if they fail. 
fromMeasure :: (Ord a, Num a, Fractional a) => Measure a -> Maybe (Prob a)
fromMeasure m
    | M.isPositive m && M.total m == 1 = Just $ Prob m
    | otherwise = Nothing

-- | View a 'M.Measure' as a probability distribution.
--
-- Variant of 'fromMeasure' where /the precondition are not checked!/
unsafeFromMeasure :: Measure a -> Prob a
unsafeFromMeasure = Prob

-- | @eval (distribution m) x@ is the probability of picking a number @<= x@.
--
-- This is known as the
-- [cumulative distribution function
-- ](https://en.wikipedia.org/wiki/Cumulative_distribution_function).
distribution :: (Ord a, Num a) => Prob a -> Piecewise (Poly a)
distribution (Prob m) = M.distribution m

-- | Construct a probability distribution from its
-- [cumulative distribution function
-- ](https://en.wikipedia.org/wiki/Cumulative_distribution_function).
--
-- Return 'Nothing' if
-- * the cumulative distribution function is not monotonicall increasing
-- * the last piece of the piecewise function is not a constant
--   equal to @1@.
fromDistribution
    :: (Ord a, Num a, Fractional a)
    => Piecewise (Poly a) -> Maybe (Prob a)
fromDistribution pieces
    | Just m <- M.fromDistribution pieces = fromMeasure m
    | otherwise = Nothing

-- | Two probability measures are equal if they have the same cumulative
-- distribution functions.
--
-- > px == py
-- >   implies
-- >   forall t. eval (distribution px) t = eval (distribution py) t
instance (Ord a, Num a) => Eq (Prob a) where
    Prob mx == Prob my = mx == my

{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}
-- | A
-- [Dirac measure](https://en.wikipedia.org/wiki/Dirac_measure)
-- at the given point @x@.
--
-- @dirac x@ is the probability distribution where @x@ occurs with certainty.
dirac :: (Ord a, Num a) => a -> Prob a
dirac = Prob . M.dirac

-- | The uniform probability distribution on the interval \( [x,y) \).
uniform :: (Ord a, Num a, Fractional a) => a -> a -> Prob a
uniform x y = Prob $ M.uniform x y

{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}
-- | The 'support' is the smallest closed, contiguous interval \( [x,y] \)
-- outside of which the probability is zero.
--
-- Returns 'Nothing' if the interval is empty.
support :: (Ord a, Num a) => Prob a -> Maybe (a, a)
support (Prob m) = M.support m

-- | Compute the
-- [expected value](https://en.wikipedia.org/wiki/Expected_value)
-- of a polynomial @f@ with respect to the given probability distribution,
-- \( E[f(X)] \).
expectation :: (Ord a, Num a, Fractional a) => Poly a -> Prob a -> a
expectation f (Prob m) = M.integrate f m

-- | Compute the first four
-- commonly used moments of a probability distribution.
moments :: (Ord a, Num a, Fractional a) => Prob a -> Moments a
moments m =
    fromExpectedPowers (ex 1, ex 2, ex 3, ex 4)
  where
    ex n = expectation (Poly.monomial n 1) m

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | Left-biased random choice.
--
-- @choice p@ is a probability distribution where
-- events from the left argument are chosen with probablity @p@
-- and events from the right argument are chosen with probability @(1-p)@.
--
-- > eval (distribution (choice p mx my)) z
-- >    = p * eval (distribution mx) z + (1-p) * eval (distribution my) z
choice :: (Ord a, Num a, Fractional a) => a -> Prob a -> Prob a -> Prob a
choice p (Prob mx) (Prob my) = Prob $
    M.add (M.scale p mx) (M.scale (1 - p) my)

-- | Translate a probability distribution along the number line.
--
-- > eval (distribution (translate y m)) x
-- >    = eval (distribution m) (x - y)
translate :: (Ord a, Num a, Fractional a) => a -> Prob a -> Prob a
translate y (Prob m) = Prob $ M.translate y m

-- | Additive convolution of two probability measures.
--
-- Properties:
--
-- > convolve (dirac x) (dirac y) = dirac (x + y)
-- >
-- > convolve mx my               =  convolve my mx
-- > translate z (convolve mx my) =  convolve (translate z mx) my
convolve
    :: (Ord a, Num a, Fractional a)
    => Prob a -> Prob a -> Prob a
convolve (Prob mx) (Prob my) = Prob $ M.convolve mx my
