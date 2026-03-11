{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
-}
module Numeric.Function.PiecewiseSpec
    ( spec
    , genInterval
    , genPiecewise
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Numeric.Function.Piecewise
    ( Piecewise
    , fromAscPieces
    , fromInterval
    , intervals
    , toAscPieces
    , translateWith
    , trim
    , zipPointwise
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Positive (..)
    , (===)
    , (.&&.)
    , arbitrary
    , frequency
    , listOf
    , property
    )

import qualified Data.Function.Class as Fun

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "Test consistency" $ do
      describe "Linear" $ do
        it "eval . translate" $ property $
            \p y x ->
                evalLinear (translateLinear y p) x
                    ===  evalLinear p (x - y)

      describe "Interval" $ do
        it "member intersect" $ property $
            \x y z ->
                member z (intersect x y)  ===  (member z x && member z y)

    describe "fromInterval" $ do
        it "zero" $ property $
            \(x :: Rational) (o :: Constant) ->
                eval (fromInterval (x, x) o) x  ===  0

        it "intervals" $ property $
            \(x :: Rational) (Positive d) (o :: Constant) ->
                let y = x + d
                in  intervals (fromInterval (x,y) o) === [(x,y)]

        it "eval" $ property $
            \(x :: Rational) (Positive d) (o :: Linear) z ->
                let y = x + d
                    p = fromInterval (x, y) o
                in 
                    eval p z
                        === (if x <= z && z < y then eval o z else 0)

    describe "mergeBy" $ do
        it "(p + negate p) trims to 0" $ property $
            \(p :: Piecewise Linear) ->
                let z = trim (p + negate p)
                in  null (toAscPieces z) === True
                    .&&. eval z 0 === 0

    describe "translateWith" $ do
        it "eval . translate" $ property $
            \(p :: Piecewise Linear) x y ->
                eval (translateWith translateLinear y p) x
                    === eval p (x - y)

    describe "zipPointwise" $ do
        it "intersects intervals" $ property $
            \p (q :: Piecewise Constant) ->
                allIntervals (zipPointwise (+) p q)
                === [ i
                    | ip <- allIntervals p
                    , iq <- allIntervals q
                    , let i = intersect ip iq
                    , i /= Empty
                    ]

        it "eval, +" $ property $
            \p (q :: Piecewise Linear) x ->
                eval (zipPointwise (+) p q) x
                === (eval p x + eval q x)

        it "eval, *" $ property $
            \p (q :: Piecewise Constant) x ->
                eval (zipPointwise (*) p q) x
                === (eval p x * eval q x)

    describe "instance Num (Piecewise Q Constant)" $ do
        it "(+)" $ property $
            \p (q :: Piecewise Constant) x ->
                eval (p + q) x
                === (eval p x + eval q x)

        it "(*)" $ property $
            \p (q :: Piecewise Constant) x ->
                eval (p * q) x
                === (eval p x * eval q x)

        it "negate" $ property $
            \(p :: Piecewise Constant) x ->
                eval (negate p) x
                === negate (eval p x)

        it "abs" $ property $
            \(p :: Piecewise Constant) x ->
                eval (abs p) x
                === abs (eval p x)

        it "signum" $ property $
            \(p :: Piecewise Constant) x ->
                eval (signum p) x
                === signum (eval p x)

{-----------------------------------------------------------------------------
    Helper types
    Constant and linear functions
------------------------------------------------------------------------------}
type Q = Rational

-- | Constant function
newtype Constant = Constant Q
    deriving (Eq, Show)

instance Num Constant where
    Constant a1 + Constant a2 = Constant (a1 + a2)
    Constant a1 * Constant a2 = Constant (a1 * a2)
    negate (Constant a) = Constant (negate a)
    abs (Constant a) = Constant (abs a)
    signum (Constant a) = Constant (signum a)
    fromInteger n = Constant (fromInteger n)

instance Fun.Function Constant where
    type instance Domain Constant = Q
    type instance Codomain Constant = Q
    eval (Constant a) _ = a

-- | Linear function with a constant and a slope
data Linear = Linear Q Q
    deriving (Eq, Show)

instance Num Linear where
    Linear a1 b1 + Linear a2 b2 = Linear (a1 + a2) (b1 + b2)
    negate (Linear a b) = Linear (negate a) (negate b)
    fromInteger n = Linear 0 (fromInteger n)

instance Fun.Function Linear where
    type instance Domain Linear = Q
    type instance Codomain Linear = Q
    eval = evalLinear

translateLinear :: Q -> Linear -> Linear
translateLinear y (Linear a b) = Linear a (b - a*y)

evalLinear :: Linear -> Q -> Q
evalLinear (Linear a b) x = a*x + b

{-----------------------------------------------------------------------------
    Helper types
    Intervals
------------------------------------------------------------------------------}
-- | Interval on the real number line.
-- This type does not represent all interval types,
-- only those that are relevant to our purposes here.
data Interval
    = All
    | Empty
    | Before Q  -- exclusive
    | After Q   -- inclusive
    | FromTo Q Q
    deriving (Eq, Show)

-- | Definition of membership.
member :: Q -> Interval -> Bool
member _ All = True
member _ Empty = False
member z (Before y) = z < y
member z (After x) = x <= z
member z (FromTo x y) = x <= z && z < y

-- | The intersection of two 'Interval' is again an 'Interval'.
intersect :: Interval -> Interval -> Interval
intersect All x = x
intersect x All = x
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Before y1) (Before y2) = Before (min y1 y2)
intersect (Before y1) (After x2) = mkFromTo x2 y1
intersect (Before y1) (FromTo x2 y2) = mkFromTo x2 (min y1 y2)
intersect (After x1) (After x2) = After (max x1 x2)
intersect (After x1) (Before y2) = mkFromTo x1 y2
intersect (After x1) (FromTo x2 y2) = mkFromTo (max x1 x2) y2
intersect (FromTo x1 y1) (Before y2) = mkFromTo x1 (min y1 y2)
intersect (FromTo x1 y1) (After x2) = mkFromTo (max x1 x2) y1
intersect (FromTo x1 y1) (FromTo x2 y2) = mkFromTo (max x1 x2) (min y1 y2)

-- | Smart constructor,
-- returns 'Empty' if the endpoint does not come after the starting point.
mkFromTo :: Q -> Q -> Interval
mkFromTo x y = if x < y then FromTo x y else Empty

-- | Return all intervals, 
allIntervals :: Fun.Domain o ~ Q => Piecewise o -> [Interval]
allIntervals pieces
    | null xs = [All]
    | otherwise = [Before xmin] <> map (uncurry FromTo) is <> [After xmax]
  where
    xs = map fst (toAscPieces pieces)
    is = zip xs (drop 1 xs)
    xmin = minimum xs
    xmax = maximum xs

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary Constant where
    arbitrary = Constant <$> arbitrary

instance Arbitrary Linear where
    arbitrary = Linear <$> arbitrary <*> arbitrary

genInterval :: Gen (Q,Q)
genInterval = do
    x <- arbitrary
    Positive d <- arbitrary
    pure (x, x + d)

genFromTo :: Gen Interval
genFromTo = uncurry FromTo <$> genInterval

instance Arbitrary Interval where
    arbitrary = frequency
        [ (1, pure All)
        , (1, pure Empty)
        , (3, Before <$> arbitrary)
        , (3, After <$> arbitrary)
        , (20, genFromTo)
        ]

-- | A list of disjoint and sorted elements.
newtype DisjointSorted a = DisjointSorted [a]
    deriving (Eq, Show)

genDisjointSorted :: Gen (DisjointSorted Rational)
genDisjointSorted =
    DisjointSorted . drop 1 . scanl (\s (Positive d) -> s + d) 0
        <$> listOf arbitrary

instance Arbitrary (DisjointSorted Rational) where
    arbitrary = genDisjointSorted

genPiecewise :: Fun.Domain o ~ Rational => Gen o -> Gen (Piecewise o)
genPiecewise gen = do
    DisjointSorted xs <- genDisjointSorted
    os <- mapM (const gen) xs
    pure $ fromAscPieces $ zip xs os

instance
    (Fun.Domain o ~ Rational, Arbitrary o)
    => Arbitrary (Piecewise o)
  where
    arbitrary = genPiecewise arbitrary
