{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
-}
module Numeric.Polynomial.SimpleSpec
    ( spec
    , genPoly
    , genPositivePoly
    ) where

import Prelude

import Data.List
    ( nub
    )
import Data.Traversable
    ( for
    )
import Numeric.Polynomial.Simple
    ( Poly
    , compareToZero
    , constant
    , convolve
    , countRoots
    , degree
    , differentiate
    , display
    , euclidianDivision
    , eval
    , fromCoefficients
    , integrate
    , isMonotonicallyIncreasingOn
    , lineFromTo
    , monomial
    , root
    , scale
    , scaleX
    , translate
    , zero
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , NonNegative (..)
    , Positive (..)
    , (===)
    , (==>)
    , (.&&.)
    , arbitrary
    , forAll
    , frequency
    , listOf
    , mapSize
    , property
    , withMaxSuccess
    )

import qualified Test.QuickCheck as QC

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "constant" $ do
        it "eval" $ property $
            \c (x :: Rational) ->
                eval (constant c) x  ===  c

    describe "scale" $ do
        it "eval" $ property $
            \a p (x :: Rational) ->
                eval (scale a p) x  ===  a * eval p x

    describe "scaleX" $ do
        it "degree" $ property $
            \(p :: Poly Rational) ->
                (degree p >= 0)
                ==> (degree (scaleX p) === 1 + degree p)

        it "eval" $ property $
            \p (x :: Rational) ->
                eval (scaleX p) x  ===  x * eval p x

        it "zero" $ withMaxSuccess 1 $ property $
            scaleX zero  ==  (zero :: Poly Rational)

    describe "(+)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p + q) x  ===  eval p x + eval q x

    describe "(*)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p * q) x  ===  eval p x * eval q x

    describe "display" $ do
        it "step == 0" $ property $
            \(l :: Rational) (Positive d) ->
                let u = l + d
                in  display zero (l, u) 0
                        === zip [l, u] (repeat 0)

        it "zero" $ property $
            \(l :: Rational) (Positive d) (Positive (n :: Integer)) ->
                let u = l + d
                    s = (u - l) / fromIntegral (min 100 n)
                in  display zero (l, u) s
                        === zip (nub ([l, l+s .. u] <> [u])) (repeat 0)

    describe "lineFromTo" $ do
        it "degree" $ property $
            \x1 (x2 :: Rational) y1 y2 ->
                let p = lineFromTo (x1, y1) (x2, y2)
                in  degree p <= 1

        it "eval" $ property $
            \x1 (x2 :: Rational) y1 y2 ->
                let p = lineFromTo (x1, y1) (x2, y2)
                in  x1 /= x2
                    ==> (eval p x1 === y1  .&&.  eval p x2 == y2)


    describe "integrate" $ do
        it "eval" $ property $
            \(p :: Poly Rational) ->
                eval (integrate p) 0  ===  0

        it "integrate . differentiate" $ property $
            \(p :: Poly Rational) ->
                integrate (differentiate p) ===  p - constant (eval p 0)

    describe "differentiate" $ do
        it "differentiate . integrate" $ property $
            \(p :: Poly Rational) ->
                differentiate (integrate p)  ===  p

        it "Leibniz rule" $ property $
            \(p :: Poly Rational) q ->
                differentiate (p * q)
                    ===  differentiate p * q + p * differentiate q

    describe "translate" $ do
        it "eval" $ property $
            \p y (x :: Rational) ->
                eval (translate y p) x  ===  eval p (x - y)

        it "differentiate" $ property $
            \p (y :: Rational) ->
                differentiate (translate y p)
                    ===  translate y (differentiate p)

    describe "euclidianDivision" $ do
        it "a = q * b + r, and  degree r < degree b" $ property $
            \a (b :: Poly Rational) ->
                let (q, r) = euclidianDivision a b in
                b /= zero ==>
                    (a  === q*b + r  .&&.  degree r < degree b)

    describe "convolve" $ do
        it "product of integrals" $ property $ mapSize (`div` 6) $
            \(NonNegative x1) (Positive d1) (NonNegative x2) (Positive d2)
              p (q :: Poly Rational) ->
                let p1 = (x1, x1 + d1, p)
                    q1 = (x2, x2 + d2, q)
                in
                    integrateInterval p1 * integrateInterval q1
                        === integratePieces (convolve p1 q1)

    describe "countRoots" $ do
        it "counts distinct roots in open interval" $ property $
            \(PolyWithRealRoots p roots) (x1 :: Rational) (Positive d) ->
                let x2 = x1 + d in
                    countRoots (x1, x2, p)
                        ===  countRoots' (x1, x2) roots

        it "handles roots at boundary" $ mapSize (`div` 2) $ property $
            \(PolyWithRealRoots p _) (x1 :: Rational) (Positive d) ->
                let x2 = x1 + d
                    xx = monomial 1 1
                    rootCount = countRoots (x1, x2, p)
                in      countRoots (x1, x2, p * (xx - constant x1))
                            ===  rootCount
                    .&&.
                        countRoots (x1, x2, p * (xx - constant x2))
                            ===  rootCount

    describe "root" $ do
        it "cubic polynomial" $ property $ mapSize (`div` 5) $
            \(x1 :: Rational) (Positive dx3) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    x2 = 0.6 * x1 + 0.4 * x3
                    x3 = x1 + dx3
                    p = (xx - constant x1) * (xx - constant x2) * (xx - constant x3)
                    l = x1 + 100 * epsilon
                    u = x3 - 100 * epsilon
                    epsilon = (x3-x1)/(1000*1000*50)
                    Just x2' = root epsilon 0 (l, u) p
                in
                    property $ abs (x2' - x2) <= epsilon

        it "cubic polynomial, midpoint" $ property $ mapSize (`div` 5) $
            \(x1 :: Rational) (Positive dx3) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    x2 = (x1 + x3) / 2
                    x3 = x1 + dx3
                    p = (xx - constant x1) * (xx - constant x2) * (xx - constant x3)
                    l = x1 + 100 * epsilon
                    u = x3 - 100 * epsilon
                    epsilon = (x3-x1)/(1000*1000*50)
                    Just x2' = root epsilon 0 (l, u) p
                in
                    abs (x2' - x2) <= epsilon

        it "high-degree polynomial, repeated root" $ property $ mapSize (`div` 5) $
            \(Positive (x1 :: Rational)) (Positive (n :: Integer)) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    p = xx*(xx - constant x1)^n
                    l = x1 - 100 * epsilon
                    u = x1 + 100 * epsilon
                    epsilon = x1/(1000*1000*50)
                    Just x2' = root epsilon 0 (l, u) p
                in
                    abs (x2' - x1) <= epsilon

    describe "isMonotonicallyIncreasingOn" $
        it "quadratic polynomial" $ property $
            \(x1 :: Rational) (Positive d) ->
                let xx = scaleX (constant 1)
                    p  = negate ((xx - constant x1) * (xx - constant x2))
                    x2 = x1 + d
                    xmid = (x1 + x2) / 2
                in
                    isMonotonicallyIncreasingOn p (x1,xmid)  ===  True

    describe "compareToZero" $ do
        it "lineFromTo" $ property $
            \(x1 :: Rational) (Positive dx) y1 (Positive dy) ->
                let x2 = x1 + dx
                    y2 = y1 + dy
                    p = lineFromTo (x1, y1) (x2, y2)
                    result
                        | y1 == 0 && y2 == 0 = Just EQ
                        | y1 >= 0 = Just GT
                        | y2 <= 0 = Just LT
                        | otherwise = Nothing
                in
                    compareToZero (x1, x2, p)
                        === result

        it "quadratic polynomial with two roots" $ property $
            \(x1 :: Rational) (Positive d) ->
                let xx = scaleX (constant 1)
                    p  = (xx - constant x1 + 1) * (xx - constant x2 - 1)
                    x2 = x1 + d
                in
                    compareToZero (x1, x2, p)  ===  Just LT

        it "quadratic polynomial + a0" $ property $
            \(x1 :: Rational) a0 ->
                let xx = scaleX (constant 1)
                    p  = (xx - constant x1)^(2 :: Int) + constant a0
                in
                    compareToZero (x1 - abs a0 - 1, x1 + abs a0 + 1, p)
                        === 
                        if a0 > 0
                            then Just GT
                            else Nothing

    describe "genPositivePoly" $
        it "eval" $ property $
            \(x :: Rational) ->
            forAll genPositivePoly $ \p ->
                eval p x > 0

    describe "genPolyWithRealRoots" $
        it "eval" $ property $
            \(PolyWithRealRoots (p :: Poly Rational) (Roots roots)) ->
                all (\x -> eval p x == 0) $ map fst roots

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Definite integral of a polynomial over an interval.
integrateInterval
    :: (Eq a, Num a, Fractional a) => (a, a, Poly a) -> a
integrateInterval (x, y, p) = eval pp y - eval pp x
  where pp = integrate p

-- | Definite integral of a sequence of polynomials over pieces.
integratePieces
    :: (Eq a, Num a, Fractional a) => [(a, Poly a)] -> a
integratePieces = sum . map integrateInterval . intervals
  where
    intervals pieces =
        [ (x, y, p)
        | ((x, p), y) <- zip pieces $ drop 1 $ map fst pieces
        ]

-- | Multiplicity of a root.
type Multiplicity = Int

-- | A list of roots with multiplicity.
newtype Roots a = Roots [(a, Multiplicity)]
    deriving (Eq, Show)

-- | Use [Vieta's theorem
-- ](https://en.wikipedia.org/wiki/Vieta%27s_formulas)
-- to convert a list of roots with mulitiplicities into
-- a polynomial with exactly those roots.
fromRoots :: (Ord a, Num a) => Roots a -> Poly a
fromRoots (Roots xms) =
    product $ map (\(r,m) -> (xx - constant r) ^ m) xms
  where
    xx = monomial 1 1

-- | Count the distinct number of real roots
-- that lie in the given, open interval.
countRoots' :: Ord a => (a, a) -> Roots a -> Int
countRoots' (xl, xr) (Roots xs) =
    length . filter (\x -> xl < x && x < xr) $ map fst xs

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
-- | Generate an arbitrary polynomial.
genPoly :: Gen (Poly Rational)
genPoly = fromCoefficients <$> listOf arbitrary

instance Arbitrary (Poly Rational) where
    arbitrary = genPoly

-- | Generate a quadratic polynomial that is positive,
-- i.e. has no real roots and is always larger than zero.
genQuadraticPositivePoly :: Gen (Poly Rational)
genQuadraticPositivePoly = do
    let xx = fromCoefficients [0, 1]
    x0 <- constant <$> arbitrary
    Positive b <- arbitrary
    pure $ (xx - x0) * (xx - x0) + constant b

-- | Generate a positive polynomial, i.e. @eval p x > 0@ for all @x@.
genPositivePoly :: Gen (Poly Rational)
genPositivePoly =
    QC.scale (`div` 3) $ product <$> listOf genQuadraticPositivePoly

-- | A list of disjoint and sorted elements.
newtype DisjointSorted a = DisjointSorted [a]
    deriving (Eq, Show)

genDisjointSorted :: Gen (DisjointSorted Rational)
genDisjointSorted = 
    DisjointSorted . drop 1 . scanl (\s (Positive d) -> s + d) 0
        <$> listOf arbitrary

instance Arbitrary (DisjointSorted Rational) where
    arbitrary = genDisjointSorted

genMultiplicity :: Gen Multiplicity
genMultiplicity =
    frequency [(20, pure 1), (2, pure 2), (2, pure 3), (1, pure 7)]

genRoots :: Gen (Roots Rational)
genRoots = do
    DisjointSorted xs <- arbitrary
    xms <- for xs $ \x -> do
        multiplicity <- genMultiplicity
        pure $ (x, multiplicity)
    pure $ Roots xms

instance Arbitrary (Roots Rational) where
    arbitrary = genRoots

-- | A polynomial with known real roots.
-- The polynomial may have additional complex roots.
data PolyWithRealRoots a = PolyWithRealRoots (Poly a) (Roots a)
    deriving (Eq, Show)

genPolyWithRealRoots :: Gen (PolyWithRealRoots Rational)
genPolyWithRealRoots = do
    roots <- QC.scale (`div` 7) $ arbitrary
    q <- QC.scale (`div` 11) $ genPositivePoly
    pure $ PolyWithRealRoots (fromRoots roots * q) roots

instance Arbitrary (PolyWithRealRoots Rational) where
    arbitrary = genPolyWithRealRoots
