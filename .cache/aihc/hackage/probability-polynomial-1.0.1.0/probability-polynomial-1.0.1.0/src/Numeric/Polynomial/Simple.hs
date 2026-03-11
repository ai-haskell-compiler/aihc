{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Polynomials and computations with them.
-}
module Numeric.Polynomial.Simple
    ( -- * Basic operations
      Poly
    , eval
    , degree
    , constant
    , zero
    , monomial
    , fromCoefficients
    , toCoefficients
    , scale
    , scaleX

      -- * Advanced operations

      -- ** Convenience
    , display
    , lineFromTo

      -- ** Algebraic
    , translate
    , integrate
    , differentiate
    , euclidianDivision
    , convolve

      -- ** Numerical
    , compareToZero
    , countRoots
    , isMonotonicallyIncreasingOn
    , root
    , squareFreeFactorisation
    ) where

import Control.DeepSeq
    ( NFData
    , NFData1
    )
import GHC.Generics
    ( Generic
    , Generic1
    )
import Math.Combinatorics.Exact.Binomial -- needed to automatically derive NFData
    ( choose
    )

import qualified Data.Function.Class as Fun

{-----------------------------------------------------------------------------
    Basic operations
------------------------------------------------------------------------------}

-- | Polynomial with coefficients in @a@.
newtype Poly a = Poly [a]
    -- INVARIANT: List of coefficients from lowest to highest degree.
    -- INVARIANT: The empty list is not allowed,
    -- the zero polynomial is represented as [0].
    deriving (Show, Generic, Generic1)

instance NFData a => NFData (Poly a)
instance NFData1 Poly

instance (Eq a, Num a) => Eq (Poly a) where
    x == y =
        toCoefficients (trimPoly x) == toCoefficients (trimPoly y)

{-| The constant polynomial.

> eval (constant a) = const a
-}
constant :: a -> Poly a
constant x = Poly [x]

-- | The zero polynomial.
zero :: Num a => Poly a
zero = constant 0

{-| Degree of a polynomial.

The degree of a constant polynomial is @0@, but
the degree of the zero polynomial is @-1@ for Euclidean division.
-}
degree :: (Eq a, Num a) => Poly a -> Int
degree x = case trimPoly x of
    Poly [0] -> -1
    Poly xs -> length xs - 1

-- | remove top zeroes
trimPoly :: (Eq a, Num a) => Poly a -> Poly a
trimPoly (Poly as) = Poly (reverse $ goTrim $ reverse as)
  where
    goTrim [] = error "Empty polynomial"
    goTrim xss@[_] = xss -- can't use dropWhile as it would remove the last zero
    goTrim xss@(x : xs) = if x == 0 then goTrim xs else xss

-- | @monomial n a@ is the polynomial @a * x^n@.
monomial :: (Eq a, Num a) => Int -> a -> Poly a
monomial n x = if x == 0 then zero else Poly (reverse (x : replicate n 0))

{-| Construct a polynomial @a0 + a1·x + …@ from
its list of coefficients @[a0, a1, …]@.
-}
fromCoefficients :: (Eq a, Num a) => [a] -> Poly a
fromCoefficients [] = zero
fromCoefficients as = trimPoly $ Poly as

{-| List the coefficients @[a0, a1, …]@
of a polynomial @a0 + a1·x + …@.
-}
toCoefficients :: Poly a -> [a]
toCoefficients (Poly as) = as

{-| Multiply the polynomial by the unknown @x@.

> eval (scaleX p) x = x * eval p x
> degree (scaleX p) = 1 + degree p  if  degree p >= 0
-}
scaleX :: (Eq a, Num a) => Poly a -> Poly a
scaleX (Poly xs)
    | xs == [0] = Poly xs -- don't shift up zero
    | otherwise = Poly (0 : xs)

{-| Scale a polynomial by a scalar.
More efficient than multiplying by a constant polynomial.

> eval (scale a p) x = a * eval p x
-}
scale :: Num a => a -> Poly a -> Poly a
scale x (Poly xs) = Poly (map (* x) xs)

-- Does not agree with naming conventions in `Data.Poly`.

{-|
   Add polynomials by simply adding their coefficients as long as both lists continue.
   When one list runs out we take the tail of the longer list (this prevents us from just using zipWith!).
   Addtion might cancel out the highest order terms, so need to trim just in case.
-}
addPolys :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
addPolys (Poly as) (Poly bs) = trimPoly (Poly (go as bs))
  where
    go [] ys = ys
    go xs [] = xs
    go (x : xs) (y : ys) = (x + y) : go xs ys

{-|
    multiply term-wise and then add (very simple - FFTs might be faster, but not for today)
    (a0 + a1x + a2x^2 + ...) * (b0 + b1x + b2x^2 ...)
    = a0 * (b0 + b1x + b2x^2 +...) + a1x * (b0 + b1x + ...)
    = (a0*b0) + (a0*b1x) + ...
              + (a1*b0x) +
                         + ...
    (may be an optimisation to be done by getting the shortest poly in the right place)
-}
mulPolys :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
mulPolys as bs = sum (intermediateSums as bs)
  where
    intermediateSums :: (Eq a, Num a) => Poly a -> Poly a -> [Poly a]
    intermediateSums _ (Poly []) = error "Second polynomial was empty"
    intermediateSums (Poly []) _ = [] -- stop when we exhaust the first list
    -- as we consume the coeffecients of the first list, we shift up the second list to increase the power under consideration
    intermediateSums (Poly (x : xs)) ys =
        scale x ys : intermediateSums (Poly xs) (scaleX ys)

{-| Algebraic operations '(+)', '(*)' and 'negate' on polynomials.

The functions 'abs' and 'signum' are undefined.
-}
instance (Eq a, Num a) => Num (Poly a) where
    (+) = addPolys
    (*) = mulPolys
    negate (Poly a) = Poly (map negate a)
    abs = undefined
    signum = undefined
    fromInteger n = Poly [Prelude.fromInteger n]

{-|
Evaluate a polynomial at a point.

> eval :: Poly a -> a -> a
-}
instance Num a => Fun.Function (Poly a) where
    type Domain (Poly a) = a
    type Codomain (Poly a) = a
    eval = eval

{-|
Evaluate a polynomial at a point.

> eval :: Poly a -> a -> a

Uses Horner's method to minimise the number of multiplications.

@
a0 + a1·x + a2·x^2 + ... + a{n-1}·x^{n-1} + an·x^n
  = a0 + x·(a1 + x·(a2 + x·(… + x·(a{n-1} + x·an)) ))
@
-}
eval :: Num a => Poly a -> a -> a
eval (Poly as) x = foldr (\ai result -> x * result + ai) 0 as

{-----------------------------------------------------------------------------
    Advanced operations
    Convenience
------------------------------------------------------------------------------}

{-|
Return a list of pairs @(x, eval p x)@ from the graph of the polynomial.
The values @x@ are from the range @(l, u)@ with uniform spacing @s@.

Specifically,

> map fst (display p (l, u) s)
>   = [l, l+s, l + 2·s, … , u'] ++ if u' == l then [] else [l]

where @u'@ is the largest number of the form @u' = l + s·k@, @k@ natural,
that still satisfies @u' < l@.
We always display the last point as well.
-}
display :: (Ord a, Eq a, Num a) => Poly a -> (a, a) -> a -> [(a, a)]
display p (l, u) s
    | s == 0 = map evalPoint [l, u]
    | otherwise = map evalPoint (l : go (l + s))
  where
    evalPoint x = (x, eval p x)
    go x
        | x >= u = [u] -- always include the last point
        | otherwise = x : go (x + s)

{-| Linear polymonial connecting the points @(x1, y1)@ and @(x2, y2)@,
assuming that @x1 ≠ x2@.

If the points are equal, we return a constant polynomial.

> let p = lineFromTo (x1, y1) (x2, y2)
>
> degree p <= 1
> eval p x1 = y1
> eval p x2 = y2
-}
lineFromTo :: (Eq a, Fractional a) => (a, a) -> (a, a) -> Poly a
lineFromTo (x1, y1) (x2, y2)
    | x1 == x2 = constant y1
    | slope == 0 = constant y1
    | otherwise = fromCoefficients [shift, slope]
  where
    -- slope of the linear function
    slope = (y2 - y1) / (x2 - x1)
    -- the constant shift is fixed by
    -- the fact that the line needs to pass through (x1,y1)
    shift = y1 - x1 * slope

{-----------------------------------------------------------------------------
    Advanced operations
    Algebraic
------------------------------------------------------------------------------}

{-| Indefinite integral of a polynomial with constant term zero.

The integral of @x^n@ is @1/(n+1)·x^(n+1)@.

> eval (integrate p) 0 = 0
> integrate (differentiate p) = p - constant (eval p 0)
-}
integrate :: (Eq a, Fractional a) => Poly a -> Poly a
integrate (Poly as) =
    -- Integrate by puting a zero constant term at the bottom and
    -- converting a x^n into a/(n+1) x^(n+1).
    -- 0 -> 0x is the first non-constant term, so we start at 1.
    -- When integrating a zero polynomial with a zero constant
    -- we get [0,0] so need to trim
    trimPoly (Poly (0 : zipWith (/) as (iterate (+ 1) 1)))

{-| Differentiate a polynomial.

We have @dx^n/dx = n·x^(n-1)@.

> differentiate (integrate p) = p
> differentiate (p * q) = (differentiate p) * q + p * (differentiate q)
-}
differentiate :: Num a => Poly a -> Poly a
differentiate (Poly []) = error "Polynomial was empty"
differentiate (Poly [_]) = zero -- constant differentiates to zero
differentiate (Poly (_ : as)) =
    -- discard the constant term, everything else noves down one
    Poly (zipWith (*) as (iterate (+ 1) 1))

{-| Convolution of two polynomials defined on bounded intervals.
Produces three contiguous pieces as a result.
-}
convolve
    :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> (a, a, Poly a) -> [(a, Poly a)]
convolve (lf, uf, Poly fs) (lg, ug, Poly gs)
    | (lf < 0) || (lg < 0) = error "Interval bounds cannot be negative"
    | (lf >= uf) || (lg >= ug) = error "Invalid interval" -- upper bounds should be strictly greater than lower bounds
    | (ug - lg) > (uf - lf) = convolve (lg, ug, Poly gs) (lf, uf, Poly fs) -- if g is wider than f, swap the terms
    | otherwise -- we know g is narrower than f
        =
        let
            -- sum a set of terms depending on an iterator k (assumed to go down to 0), where each term is a k-dependent
            -- polynomial with a k-dependent multiplier
            sumSeries k mulFactor poly = sum [mulFactor n `scale` poly n | n <- [0 .. k]]

            -- the inner summation has a similar structure each time
            innerSum m n term k = sumSeries (m + k + 1) innerMult (\j -> monomial (m + n + 1 - j) (term j))
              where
                innerMult j =
                    fromIntegral
                        (if even j then (m + k + 1) `choose` j else negate ((m + k + 1) `choose` j))

            convolveMonomials m n innerTerm = sumSeries n (multiplier m n) (innerTerm m n)
              where
                multiplier p q k =
                    fromIntegral (if even k then q `choose` k else negate (q `choose` k))
                        / fromIntegral (p + k + 1)

            {-
                For each term, clock through the powers of each polynomial to give convolutions of monomials, which we sum.
                We extract each coefficient of each polynomial, together with an integer recording their position (i.e. power of x),
                and multiply the coefficients together with the new polynomial generated by convolving the monomials.
            -}
            makeTerm f =
                sum
                    [ (a * b) `scale` convolveMonomials m n f
                    | (m, a) <- zip [0 ..] fs
                    , (n, b) <- zip [0 ..] gs
                    ]

            firstTerm =
                makeTerm (\m n k -> innerSum m n (lg ^) k - monomial (n - k) (lf ^ (m + k + 1)))

            secondTerm = makeTerm (\m n -> innerSum m n (\k -> lg ^ k - ug ^ k))

            thirdTerm =
                makeTerm (\m n k -> monomial (n - k) (uf ^ (m + k + 1)) - innerSum m n (ug ^) k)
        in
            {-
                When convolving distributions, both distributions will start at 0 and so there will always be a pair of intervals
                with lg = lf = 0, so we don't need to add an initial zero piece.
                We must have lf + lg < lf + ug due to initial interval validity check. However, it's possible that lf + ug = uf + lg, so
                we need to test for a redundant middle interval
            -}
            if lf + ug == uf + lg
                then [(lf + lg, firstTerm), (uf + lg, thirdTerm), (uf + ug, zero)]
                else
                    [ (lf + lg, firstTerm)
                    , (lf + ug, secondTerm)
                    , (uf + lg, thirdTerm)
                    , (uf + ug, zero)
                    ]

{-| Translate the argument of a polynomial by summing binomial expansions.

> eval (translate y p) x = eval p (x - y)
-}
translate :: forall a. (Fractional a, Eq a, Num a) => a -> Poly a -> Poly a
translate y (Poly ps) =
    sum
      [ b `scale` binomialExpansion n
      | (n, b) <- zip [0 ..] ps
      ]
  where
    -- binomialTerm n k = coefficient of x^k in the expensation of (x - y)^n
    binomialTerm :: Integer -> Integer -> a
    binomialTerm n k = fromInteger (n `choose` k) * (-y) ^ (n - k)

    -- binomialExpansion n = (x - y)^n  expanded as a polyonial in x
    binomialExpansion :: Integer -> Poly a
    binomialExpansion n = Poly (map (binomialTerm n) [0 .. n])

{-|
[Euclidian division of polynomials
](https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_division)
takes two polynomials @a@ and @b ≠ 0@,
and returns two polynomials, the quotient @q@ and the remainder @r@,
such that

> a = q * b + r
> degree r < degree b
-}
euclidianDivision
    :: forall a. (Fractional a, Eq a, Ord a)
    => Poly a -> Poly a -> (Poly a, Poly a)
euclidianDivision pa pb
    | pb == zero = error "Division by zero polynomial"
    | otherwise = goDivide (zero, pa)
  where
    degB = degree pb

    -- Coefficient of the highest power term
    leadingCoefficient :: Poly a -> a
    leadingCoefficient (Poly x) = last x

    lcB = leadingCoefficient pb

    goDivide :: (Poly a, Poly a) -> (Poly a, Poly a)
    goDivide (q, r)
        | degree r < degB = (q, r)
        | otherwise = goDivide (q + s, r - s * pb)
      where
        s = monomial (degree r - degB) (leadingCoefficient r / lcB)

{-----------------------------------------------------------------------------
    Advanced operations
    Numerical
------------------------------------------------------------------------------}
{-|
@'countRoots' (x1, x2, p)@ returns the number of /distinct/ real roots
of the polynomial on the open interval \( (x_1, x_2) \).

(Roots with higher multiplicity are each counted as a single distinct root.)

This function uses [Sturm's theorem
](https://en.wikipedia.org/wiki/Sturm%27s_theorem),
with special provisions for roots on the boundary of the interval.
-}
countRoots :: (Fractional a, Ord a) => (a, a, Poly a) -> Int
countRoots (l, r, p) =
    countRoots' $ (p `factorOutRoot` l) `factorOutRoot` r
  where
    -- we can now assume that the polynomial has no roots at the boundary
    countRoots' q = case degree q of
        -- q is the zero polynomial, so it doesn't *cross* zero
        -1 -> 0
        -- q is a non-zero constant polynomial - no root
        0 -> 0
        -- q is a linear polynomial,
        1 -> if eval q l * eval q r < 0 then 1 else 0
        -- q has degree 2 or more so we can construct the Sturm sequence
        _ -> countRootsSturm (l, r, q)

-- | Given a polynomial \( p(x) \) and a value \( a \),
-- this functions factors out the polynomial \( (x-a)^m \),
-- where \( m \) is the highest power where this polynomial
-- divides \( p(x) \) without remainder.
--
-- * If the value \( a \) is a root of the polynomial,
--   then \( m \) is the multiplicity of the root.
-- * If the value \( a \) is not a root, then
--   \( m = 0 \) and the function returns \( p (x) \).
--
-- In other words, this function returns a polynomial \( q (x) \)
-- such that
--
-- \( p(x) = q(x)·(x - a)^m \)
--
-- where \( q(a) ≠ 0 \).
-- If the polynomial \( p(x) \) is identically 'zero',
-- we return 'zero' as well.
factorOutRoot :: (Fractional a, Ord a) => Poly a -> a -> Poly a
factorOutRoot p0 x0
    | p0 == zero = zero
    | otherwise = go p0
  where
    go p
        | eval p x0 == 0 = factorOutRoot pDividedByXMinusX0 x0
        | otherwise = p
      where
        xMinusX0 = monomial 1 1 - constant x0
        (pDividedByXMinusX0, _) = p `euclidianDivision` xMinusX0

{-|
@'countRootsSturm' (x1, x2, p)@ returns the number of /distinct/ real roots
of the polynomial @p@ on the half-open interval \( (x_1, x_2] \),
under the following assumptions:

* @'degree' p >= 2@
* neither \( x_1 \) nor \( x_2 \) are multiple roots of \( p(x) \).

This function is an implementation of [Sturm's theorem
](https://en.wikipedia.org/wiki/Sturm%27s_theorem).
-}
countRootsSturm :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> Int
countRootsSturm (l, r, p) =
    -- p has degree 2 or more so we can construct the Sturm sequence
    signVariations psl - signVariations psr
  where
    ps = reversedSturmSequence p
    psl = map (flip eval l) ps
    psr = map (flip eval r) ps

{-| Number of sign variations in a list of real numbers.

Given a list @c0, c1, c2, . . . ck@,
then a sign variation (or sign change) in the sequence
is a pair of indices @i < j@ such that @ci*cj < 0@,
and either @j = i + 1@ or @ck = 0@ for all @@ such that @i < k < j@.
-}
signVariations :: (Fractional a, Ord a) => [a] -> Int
signVariations xs =
    length (filter (< 0) pairsMultiplied)
  where
    -- we simply remove zero elements to implement the clause
    -- "ck = 0 for all k such that i < k < j"
    zeroesRemoved = filter (/= 0) xs
    pairsMultiplied = zipWith (*) zeroesRemoved (drop 1 zeroesRemoved)

{-|
Construct the [Sturm sequence
](https://en.wikipedia.org/wiki/Sturm%27s_theorem)
of a given polynomial @p@. The Sturm sequence is given by the polynomials

> p0 = p
> p1 = differentiate p
> p{i+1} = - rem(p{i-1}, pi)

where @rem@ denotes the remainder under 'euclidianDivision'.
We truncate the list when one of the @pi = 0@.

For ease of implementation, we

* construct the 'reverse' of the Sturm sequence.
  This does not affect the number of sign variations that the usage site
  will be interested in.

* assume that the @degree p >= 1@.
-}
reversedSturmSequence :: (Fractional a, Ord a) => Poly a -> [Poly a]
reversedSturmSequence p =
    go [differentiate p, p]
  where
    -- Note that this is called with a list of length 2 and grows the list,
    -- so we don't need to match all cases.
    go ps@(pI : pIminusOne : _)
        | remainder == zero = ps
        | otherwise = go (negate remainder : ps)
      where
        remainder = snd $ euclidianDivision pIminusOne pI
    go _ = error "reversedSturmSequence: impossible"

{-| Check whether a polynomial is monotonically increasing on
a given interval.
-}
isMonotonicallyIncreasingOn
    :: (Fractional a, Eq a, Ord a) => Poly a -> (a, a) -> Bool
isMonotonicallyIncreasingOn p (x1, x2) =
    eval p x1 <= eval p x2
        && countRoots (x1, x2, differentiate p) == 0

{-|
Measure whether or not a polynomial is consistently above or below zero,
or equals zero.

Need to consider special cases where there is a root at a boundary point.
-}
compareToZero :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> Maybe Ordering
compareToZero (l, u, p)
    | l >= u = error "Invalid interval"
    | p == zero = Just EQ
    | lower * upper < 0 = Nothing -- quick test to eliminate simple cases
    | countRoots (l, u, p) > 0 = Nothing -- polynomial crosses zero
    -- since the polynomial has no roots, the comparison is detmined by the boundary values
    | lower == 0 = Just (compare upper lower)
    | upper == 0 = Just (compare lower upper)
    | lower > 0 = Just GT -- upper must also be > 0 due to the lack of roots
    | otherwise = Just LT -- upper and lower both < 0 due to the lack of roots
  where
    lower = eval p l
    upper = eval p u

{-|
Find a root of a polynomial in a given interval.

Return 'Nothing' if the polynomial does not have a root in the given interval.

We find the root by first forming the square-free factorisation of the polynomial,
to eliminate repeated roots. One of the factors may have a root in the interval,
so we count roots for each factor until we find the one with a root in the interval.
Then we use the bisection method to find the root,
repeatedly halving the interval in which the root must lie
until its width is less than the specified precision.
Constant and linear polynomials, @degree p <= 1@, are treated as special cases.
-}
findRoot
    :: forall a. (Fractional a, Eq a, Num a, Ord a) => a -> (a, a) -> Poly a -> Maybe a
findRoot precision (lower, upper) poly =
    if null rootFactors
        then Nothing
        else getRoot precision (lower, upper) (head rootFactors)
  where
    rootFactors =
        filter (\x -> countRoots (lower, upper, x) /= 0) (squareFreeFactorisation poly)
    -- getRoot :: forall a. (Fractional a, Eq a, Num a, Ord a) => a -> (a, a) -> Poly a -> Maybe a
    getRoot eps (l, u) p
        -- if the polynomial is zero, the whole interval is a root, so return the basepoint
        | degp < 0 = Just l
        -- if the poly is a non-zero constant, no root is present
        | degp == 0 = Nothing
        -- if the polynomial has degree 1, can calculate the root exactly
        | degp == 1 = Just (-(head ps / last ps)) -- p0 + p1x = 0 => x = -p0/p1
        | eps <= 0 = error "Invalid precision value"
        | otherwise = bisect eps (l, u) (eval p l, eval p u) p
      where
        ps = toCoefficients p
        degp = degree p
        {- We bisect the interval exploiting the Intermediate Value Theorem:
        if a polynomial has different signs at the ends of an interval, it must be zero somewhere in the interval.
        If there is no change of sign, use countRoots to find which side of the interval the root is on.
        -}
        bisect
            :: (Fractional a, Eq a, Num a, Ord a) => a -> (a, a) -> (a, a) -> Poly a -> Maybe a
        bisect e (x, y) (px, py) p'
            -- if we already have a root, choose it
            | px == 0 = Just x
            | py == 0 = Just y
            | pmid == 0 = Just mid
            -- when the interval is small enough, stop:
            -- the root is in this interval, so take the mid point
            | width <= e = Just mid
            -- choose the lower half, if the polynomial has different signs at the ends
            | signum px /= signum pmid = bisect e (x, mid) (px, pmid) p'
            -- choose the upper half, if the polynomial has different signs at the ends
            | signum py /= signum pmid = bisect e (mid, y) (pmid, py) p'
            -- no sign change found, so we resort to counting roots
            | countRoots (x, mid, p') > 0 = bisect e (x, mid) (px, pmid) p'
            | countRoots (mid, y, p') > 0 = bisect e (mid, y) (pmid, py) p'
            | otherwise = Nothing
          where
            width = y - x
            mid = x + width / 2
            pmid = eval p' mid

{-| We are seeking the point at which a polynomial has a specific value.:
subtract the value we are looking for so that we seek a zero crossing
-}
root -- TODO: this should probably be called something else such as 'findCrossing'
    :: (Ord a, Num a, Eq a, Fractional a)
    => a
    -> a
    -> (a, a)
    -> Poly a
    -> Maybe a
root e x (l, u) p = findRoot e (l, u) (p - constant x)

-- | Greatest monic common divisor of two polynomials.
gcdPoly
    :: forall a. (Fractional a, Eq a, Num a, Ord a) => Poly a -> Poly a -> Poly a
gcdPoly a b = if b == zero then a else makeMonic (gcdPoly b (polyRemainder a b))
  where
    makeMonic :: Poly a -> Poly a
    makeMonic (Poly as) = scale (1 / last as) (Poly as)
    polyRemainder :: Poly a -> Poly a -> Poly a
    polyRemainder x y = snd (euclidianDivision x y)

{-|
We compute the square-free factorisation of a polynomial using Yun's algorithm.
Yun, David Y.Y. (1976). "On square-free decomposition algorithms".
SYMSAC '76 Proceedings of the third ACM Symposium on Symbolic and Algebraic Computation.
Association for Computing Machinery. pp. 26–35. doi:10.1145/800205.806320. ISBN 978-1-4503-7790-4. S2CID 12861227.
https://dl.acm.org/doi/10.1145/800205.806320
G <- gcd (P, P')
C1 <- P / G
D1 <- P' / G - C1'
until Ci = 1 do
    Pi <- gcd (Ci, Di)
    Ci+1 <- Ci/Pi
    Di+1 <- Di / Ai - Ci+1'
-}
squareFreeFactorisation
    :: (Fractional a, Eq a, Num a, Ord a) => Poly a -> [Poly a]
squareFreeFactorisation p = 
    -- if p has degree <= 1 it can have no factors but itself
    if degree p <= 1 then [p] else go c1 d1  
  where
    diffP = differentiate p
    g0 = gcdPoly p diffP
    c1 = p `divide` g0
    d1 = (diffP `divide` g0) - differentiate c1
    divide x y = fst (euclidianDivision x y)
    go c d
        | degree c  == 0 = [] -- terminate the recursion
        | degree a' == 0 = go c' d' -- skip over constant polynomials
        | otherwise = a' : go c' d'
      where
        a' = gcdPoly c d
        c' = c `divide` a'
        d' = (d `divide` a') - differentiate c'
