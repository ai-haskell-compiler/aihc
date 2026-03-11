{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Discrete, finite signed measures on the number line.
-}
module Numeric.Measure.Discrete
    ( -- * Type
      Discrete
    , fromMap
    , toMap
    , zero
    , dirac
    , distribution

    -- * Observations
    , total
    , integrate

    -- * Operations, numerical
    , add
    , scale
    , translate
    , beforeOrAt
    , after
    , convolve
    ) where

import Data.List
    ( scanl'
    )
import Data.Map
    ( Map
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Polynomial.Simple
    ( Poly
    )

import qualified Data.Map.Strict as Map
import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A discrete, finite
-- [signed measure](https://en.wikipedia.org/wiki/Signed_measure)
-- on the number line.
newtype Discrete a = Discrete (Map a a)
    -- INVARIANT: All values are non-zero.
    deriving (Show)

-- | Internal.
-- Remove all zero values.
trim :: (Ord a, Num a) => Map a a -> Map a a
trim m = Map.filter (/= 0) m

-- | Two measures are equal if they yield the same measures on every set.
--
-- > mx == my
-- >   implies
-- >   forall t. eval (distribution mx) t = eval (distribution my) t
instance (Ord a, Num a) => Eq (Discrete a) where
    Discrete mx == Discrete my = mx == my

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | The measure that assigns @0@ to every set.
zero :: Num a => Discrete a
zero = Discrete Map.empty

-- | A
-- [Dirac measure](https://en.wikipedia.org/wiki/Dirac_measure)
-- at the given point @x@.
--
-- > total (dirac x) = 1
dirac :: (Ord a, Num a) => a -> Discrete a
dirac x = Discrete (Map.singleton x 1)

-- | Construct a discrete measure
-- from a collection of points and their measures.
fromMap :: (Ord a, Num a) => Map a a -> Discrete a
fromMap = Discrete . trim

-- | Decompose the discrete measure into a collection of points
-- and their measures.
toMap :: Num a => Discrete a -> Map a a
toMap (Discrete m) = m

-- | The total of the measure applied to the set of real numbers.
total :: Num a => Discrete a -> a
total (Discrete m) = sum m

-- | Integrate a function @f@ with respect to the given measure @m@,
-- \( \int f(x) dm(x) \).
integrate :: (Ord a, Num a) => (a -> a) -> Discrete a -> a
integrate f (Discrete m) = sum $ Map.mapWithKey (\x w -> f x * w) m

-- | @eval (distribution m) x@ is the measure of the interval \( (-∞, x] \).
--
-- This is known as the [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_%28measure_theory%29).
distribution :: (Ord a, Num a) => Discrete a -> Piecewise (Poly a)
distribution (Discrete m) =
    Piecewise.fromAscPieces
    $ zipWith (\(x,_) s -> (x,Poly.constant s)) diracs steps
  where
    diracs = Map.toAscList m
    steps = tail $ scanl' (+) 0 $ map snd diracs

-- | Add two measures.
--
-- > total (add mx my) = total mx + total my
add :: (Ord a, Num a) => Discrete a -> Discrete a -> Discrete a
add (Discrete mx) (Discrete my) =
    Discrete $ trim $ Map.unionWith (+) mx my

-- | Scale a measure by a constant.
--
-- > total (scale a mx) = a * total mx
scale :: (Ord a, Num a) => a -> Discrete a -> Discrete a
scale 0 (Discrete _) = Discrete Map.empty
scale s (Discrete m) = Discrete $ Map.map (s *) m

-- | Translate a measure along the number line.
--
-- > eval (distribution (translate y m)) x
-- >    = eval (distribution m) (x - y)
translate :: (Ord a, Num a) => a -> Discrete a -> Discrete a
translate y (Discrete m) = Discrete $ Map.mapKeys (y +) m

-- | Intersect a measure with the interval @(-∞, x]@.
--
-- The measure of the interval @(-∞, t]@ with @beforeOrAt x m@ is the same as
-- the measure of the intersection @(-∞, t] ∩ (-∞, x]@ with @m@. 
beforeOrAt :: (Ord a, Num a) => a -> Discrete a -> Discrete a
beforeOrAt x (Discrete m) = Discrete $ Map.filterWithKey (\t _ -> t <= x) m

-- | Intersect a measure with the interval @(x, +∞)@.
--
-- The measure of the interval @(-∞, t]@ with @after x m@ is the same as
-- the measure of the intersection @(-∞, t] ∩ (x, +∞)@ with @m@. 
after :: (Ord a, Num a) => a -> Discrete a -> Discrete a
after x (Discrete m) = Discrete $ Map.filterWithKey (\t _ -> x < t) m

-- | Additive convolution of two measures.
--
-- Properties:
--
-- > convolve (dirac x) (dirac y) = dirac (x + y)
convolve :: (Ord a, Num a) => Discrete a -> Discrete a -> Discrete a
-- >
-- > convolve mx my               =  convolve my mx
-- > convolve (add mx my) mz      =  add (convolve mx mz) (convolve my mz)
-- > translate z (convolve mx my) =  convolve (translate z mx) my
-- > total (convolve mx my)       =  total mx * total myconvolve :: (Ord a, Num a) => Discrete a -> Discrete a -> Discrete a
convolve (Discrete mx) (Discrete my) =
    Discrete $ trim $ Map.fromListWith (+)
        [ (x + y, wx * wy)
        | (x,wx) <- Map.toList mx
        , (y,wy) <- Map.toList my
        ]
