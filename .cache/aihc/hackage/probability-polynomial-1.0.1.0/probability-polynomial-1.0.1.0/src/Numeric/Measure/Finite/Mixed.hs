{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Finite signed measures on the number line.
-}
module Numeric.Measure.Finite.Mixed
    ( -- * Type
      Measure
    , zero
    , dirac
    , uniform
    , distribution
    , fromDistribution

      -- * Observations
    , total
    , support
    , isPositive
    , integrate

      -- * Operations, numerical
    , add
    , scale
    , translate
    , beforeOrAt
    , after
    , convolve
    ) where

import Data.Function.Class
    ( Function (..)
    )
import Data.List
    ( scanl'
    )
import Control.DeepSeq
    ( NFData
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Polynomial.Simple
    ( Poly
    , constant
    )

import qualified Data.Map.Strict as Map
import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Measure.Discrete as D
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A finite
-- [signed measure](https://en.wikipedia.org/wiki/Signed_measure)
-- on the number line.
newtype Measure a = Measure (Piecewise (Poly a))
    -- INVARIANT: Adjacent pieces contain distinct objects.
    -- INVARIANT: The last piece is a constant polynomial,
    --            so that the measure is finite.
    deriving (Show, NFData)

-- | @eval (distribution m) x@ is the measure of the interval \( (-∞, x] \).
--
-- This is known as the [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_%28measure_theory%29).
distribution :: (Ord a, Num a) => Measure a -> Piecewise (Poly a)
distribution (Measure p) = p

-- | Construct a signed measure from its
-- [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_%28measure_theory%29).
--
-- Return 'Nothing' if the measure is not finite,
-- that is if the last piece of the piecewise function is not constant.
fromDistribution
    :: (Ord a, Num a)
    => Piecewise (Poly a) -> Maybe (Measure a)
fromDistribution pieces
    | isEventuallyConstant pieces = Just $ Measure $ trim pieces
    | otherwise = Nothing

-- | Test whether a piecewise polynomial is constant as x -> ∞.
isEventuallyConstant :: (Ord a, Num a) => Piecewise (Poly a) -> Bool
isEventuallyConstant pieces
    | null xpolys = True
    | otherwise = (<= 0) . Poly.degree . snd $ last xpolys
  where
    xpolys = Piecewise.toAscPieces pieces

-- | Internal.
-- Join all intervals whose polynomials are equal.
trim :: (Ord a, Num a) => Piecewise (Poly a) -> Piecewise (Poly a)
trim = Piecewise.trim

-- | Two measures are equal if they yield the same measures on every set.
--
-- > mx == my
-- >   implies
-- >   forall t. eval (distribution mx) t = eval (distribution my) t
instance (Ord a, Num a) => Eq (Measure a) where
    Measure mx == Measure my =
        Piecewise.toAscPieces mx == Piecewise.toAscPieces my

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | The measure that assigns @0@ to every set.
zero :: Num a => Measure a
zero = Measure Piecewise.zero

-- | A
-- [Dirac measure](https://en.wikipedia.org/wiki/Dirac_measure)
-- at the given point @x@.
--
-- > total (dirac x) = 1
dirac :: (Ord a, Num a) => a -> Measure a
dirac x = Measure $ Piecewise.fromAscPieces [(x, Poly.constant 1)]

-- | The probability measure of a uniform probability distribution
-- in the interval \( [x,y) \).
--
-- > total (uniform x y) = 1
uniform :: (Ord a, Num a, Fractional a) => a -> a -> Measure a
uniform x y = Measure $ case compare x y of
    EQ -> Piecewise.fromAscPieces [(x, 1)]
    _  -> Piecewise.fromAscPieces [(low, poly), (high, 1)]
  where
    low = min x y
    high = max x y
    poly = Poly.lineFromTo (low, 0) (high, 1)

-- | The total of the measure applied to the set of real numbers.
total :: (Ord a, Num a) => Measure a -> a
total (Measure p) =
    case Piecewise.toAscPieces p of
        [] -> 0
        ps -> eval (snd (last ps)) 0

-- | The 'support' is the smallest closed, contiguous interval \( [x,y] \)
-- outside of which the measure is zero.
--
-- Returns 'Nothing' if the interval is empty.
support :: (Ord a, Num a) => Measure a -> Maybe (a, a)
support (Measure pieces) =
    case Piecewise.toAscPieces pieces of
        [] -> Nothing
        ps -> Just (fst $ head ps, fst $ last ps)

-- | Check whether a signed measure is positive.
--
-- A signed measure is /positive/ if the measure of any set
-- is nonnegative. In other words a positive signed measure
-- is just a measure in the ordinary sense.
--
-- This test is nontrivial, as we have to check that the distribution
-- function is monotonically increasing.
isPositive :: (Ord a, Num a, Fractional a) => Measure a -> Bool
isPositive (Measure m) = go 0 $ Piecewise.toAscPieces m
  where
    go _ [] =
        True
    go before ((x, o) : []) =
        eval before x <= eval o x
    go before ((x1, o) : xos@((x2, _) : _)) =
        (eval before x1 <= eval o x1)
        && Poly.isMonotonicallyIncreasingOn o (x1,x2)
        && go o xos

{-----------------------------------------------------------------------------
    Operations
    Numerical
------------------------------------------------------------------------------}
-- | Add two measures.
--
-- > total (add mx my) = total mx + total my
add :: (Ord a, Num a) => Measure a -> Measure a -> Measure a
add (Measure mx) (Measure my) =
    Measure $ trim $ Piecewise.zipPointwise (+) mx my

-- | Scale a measure by a constant.
--
-- > total (scale a mx) = a * total mx
scale :: (Ord a, Num a) => a -> Measure a -> Measure a
scale 0 (Measure _) = zero
scale x (Measure m) = Measure $ Piecewise.mapPieces (Poly.scale x) m

-- | Translate a measure along the number line.
--
-- > eval (distribution (translate y m)) x
-- >    = eval (distribution m) (x - y)
translate :: (Ord a, Num a, Fractional a) => a -> Measure a -> Measure a
translate y (Measure m) =
    Measure $ Piecewise.translateWith Poly.translate y m

{-----------------------------------------------------------------------------
    Operations
    Intersection
------------------------------------------------------------------------------}
-- | Intersect a measure with the interval @(-∞, x]@.
--
-- The measure of the interval @(-∞, t]@ with @beforeOrAt x m@ is the same as
-- the measure of the intersection @(-∞, t] ∩ (-∞, x]@ with @m@. 
beforeOrAt :: (Ord a, Num a) => a -> Measure a -> Measure a
beforeOrAt x (Measure m) =
    case Piecewise.toAscPieces m of
        [] -> zero
        ((x1, _):_) ->
            let indicatorToX = Piecewise.fromInterval (x1,x) 1
                scaledIndicatorAfterX v =
                    Piecewise.fromAscPieces [(x, constant v)]
            in  Measure
                $ trim
                $ indicatorToX * m + scaledIndicatorAfterX (eval m x)

-- | Intersect a measure with the interval @(x, +∞)@.
--
-- The measure of the interval @(-∞, t]@ with @after x m@ is the same as
-- the measure of the intersection @(-∞, t] ∩ (x, +∞)@ with @m@. 
after :: (Ord a, Num a) => a -> Measure a -> Measure a
after x (Measure m) =
    let scaledIndicatorAfterX v = Piecewise.fromAscPieces [(x, constant v)]
    in  Measure
        $ trim
        $ scaledIndicatorAfterX 1 * m
            - scaledIndicatorAfterX (eval m x)

{-----------------------------------------------------------------------------
    Operations
    Decomposition into continuous and discrete measures,
    needed for convolution.
------------------------------------------------------------------------------}
-- | Measure that is absolutely continuous
-- with respect to the Lebesgue measure,
-- Represented via its distribution function.
newtype Continuous a = Continuous { unContinuous :: Piecewise (Poly a) }
    -- INVARIANT: The last piece is @Poly.constant p@ for some @p :: a@.

-- | Density function (Radon–Nikodym derivative) of an absolutely
-- continuous measure.
newtype Density a = Density (Piecewise (Poly a))
    -- INVARIANT: The last piece is @Poly.constant 0@.

-- | Density function of an absolutely continuous measure.
toDensity
    :: (Ord a, Num a, Fractional a)
    => Continuous a -> Density a
toDensity = Density . Piecewise.mapPieces Poly.differentiate . unContinuous

-- | Decompose a mixed measure into
-- a continuous measure and a discrete measure.
-- See also [Lebesgue's decomposition theorem
-- ](https://en.wikipedia.org/wiki/Lebesgue%27s_decomposition_theorem)
decompose
    :: (Ord a, Num a, Fractional a)
    => Measure a -> (Continuous a, D.Discrete a)
decompose (Measure m) =
    ( Continuous $ trim $ Piecewise.fromAscPieces withoutJumps
    , D.fromMap $ Map.fromList jumps
    )
  where
    pieces = Piecewise.toAscPieces m

    withoutJumps =
        zipWith (\(x,o) j -> (x, o - Poly.constant j)) pieces totalJumps
    totalJumps = tail $ scanl' (+) 0 $ map snd jumps

    jumps = go 0 pieces
      where
        go _ [] = []
        go prev ((x,o) : xos) =
            (x, Poly.eval o x - Poly.eval prev x) : go o xos

{-----------------------------------------------------------------------------
    Observations
    Integration
------------------------------------------------------------------------------}
-- | Integrate a polynomial @f@ with respect to the given measure @m@,
-- \( \int f(x) dm(x) \).
integrate :: (Ord a, Num a, Fractional a) => Poly a -> Measure a -> a
integrate f m =
    integrateContinuous f continuous
    + D.integrate (eval f) discrete
  where
    (continuous, discrete) = decompose m

-- | Integrate a polynomial over an absolutely continuous measure.
integrateContinuous
    :: (Ord a, Num a, Fractional a)
    => Poly a -> Continuous a -> a
integrateContinuous f gg
    | null gpieces = 0
    | otherwise = sum $ map integrateOverInterval $ integrands
  where
    Density g = toDensity gg
    gpieces = Piecewise.toAscPieces g

    -- Pieces on the bounded intervals
    boundedPieces xos =
        zipWith (\(x1,o) (x2,_) -> ((x1, x2), o)) xos (drop 1 xos)

    integrands = [ (x12, f * o) | (x12, o) <- boundedPieces gpieces ]

    integrateOverInterval ((x1, x2), p) =
        eval pp x2 - eval pp x1
      where
        pp = Poly.integrate p

{-----------------------------------------------------------------------------
    Operations
    Convolution
------------------------------------------------------------------------------}
{-$ NOTE [Convolution]

In order to compute a convolution,
we convolve a density with the distribution function.

Let $f$ denote a density, which can be continuous or a Dirac delta.
Let $G$ denote a distribution function.
Let $H = f * G$ be the result of the convolution.
It can be shown that this is the distribution function of the
convolution of the densities, $h = f * g$.

The formula for convolution is

$ H(y) = ∫ f(y - x) G(x) dx = ∫ f (x) G(y - x) dx$.

When $f$ is a sum of delta functions, $f = Σ w_j delta_{x_j}(x)$,
this integral becomes ($y - x = x_j$ => $x = y - x_j$)

$ H(y) = Σ w_j G(y - x_j) $.

When $f$ is a piecewise polynomial, we can convolve the pieces.

When convolving with a distribution function, the final piece
will be a constant $g_n$ on the interval $[x_n,∞)$.
In this case, the convolution is given by

\[
H(y)
    = ∫ f (x) G(y - x) dx
    = ∫_{ -∞}^{y-x_n} f(x) g_n dx
    = g_n F(y-x_n)
\]

where $F$ is the distribution function of the density $f$.
-}

-- | Convolve a discrete measure with a mixed measure.
--
-- See NOTE [Convolution].
convolveDiscrete
    :: (Ord a, Num a, Fractional a)
    => D.Discrete a -> Measure a -> Measure a
convolveDiscrete f gg =
    foldr add zero
        [ scale w (translate x gg)
        | (x, w) <- Map.toAscList $ D.toMap f
        ]

-- | Convolve an absolutely continuous measure with a mixed measure.
--
-- See NOTE [Convolution].
convolveContinuous
    :: (Ord a, Num a, Fractional a)
    => Continuous a -> Measure a -> Measure a
convolveContinuous (Continuous ff) (Measure gg)
    | null ffpieces = zero
    | null ggpieces = zero
    | otherwise = Measure $ trim $ boundedConvolutions + lastConvolution
  where
    ffpieces = Piecewise.toAscPieces ff
    ggpieces = Piecewise.toAscPieces gg

    Density f = toDensity (Continuous ff)
    fpieces = Piecewise.toAscPieces f

    -- Pieces on the bounded intervals
    boundedPieces xos =
        zipWith (\(x,o) (y,_) -> (x, y, o)) xos (drop 1 xos)

    boundedConvolutions =
        sum $
            [ Piecewise.fromAscPieces (Poly.convolve fo ggo)
            | fo <- boundedPieces fpieces
            , ggo <- boundedPieces ggpieces
            ]

    (xlast, plast) = last ggpieces
    glast = case Poly.toCoefficients plast of
        [] -> 0
        (a0:_) -> a0
    lastConvolution =
        Piecewise.mapPieces (Poly.scale glast)
        $ Piecewise.translateWith Poly.translate xlast ff

-- | Additive convolution of two measures.
--
-- Properties:
--
-- > convolve (dirac x) (dirac y) = dirac (x + y)
-- >
-- > convolve mx my               =  convolve my mx
-- > convolve (add mx my) mz      =  add (convolve mx mz) (convolve my mz)
-- > translate z (convolve mx my) =  convolve (translate z mx) my
-- > total (convolve mx my)       =  total mx * total my
convolve
    :: (Ord a, Num a, Fractional a)
    => Measure a -> Measure a -> Measure a
convolve mx my =
    add (convolveContinuous contx my) (convolveDiscrete deltax my)
  where
    (contx, deltax) = decompose mx
