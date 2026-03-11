{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
-}
module Numeric.Measure.ProbabilitySpec
    ( spec
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Data.Ratio
    ( (%)
    )
import Numeric.Polynomial.SimpleSpec
    ( genPositivePoly
    )
import Numeric.Measure.Probability
    ( Prob
    , choice
    , convolve
    , dirac
    , distribution
    , expectation
    , fromDistribution
    , fromMeasure
    , unsafeFromMeasure
    , measure
    , moments
    , support
    , translate
    , uniform
    )
import Numeric.Probability.Moments
    ( Moments (..)
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
    , arbitrary
    , choose
    , chooseInteger
    , frequency
    , getSize
    , mapSize
    , oneof
    , property
    , scale
    , vectorOf
    )

import qualified Numeric.Measure.Finite.Mixed as M
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "uniform" $ do
        it "support" $ property $
            \(x :: Rational) y ->
                support (uniform x y)  ===  Just (min x y, max x y)

        it "distribution at midpoint" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y ==>
                eval (distribution (uniform x y)) ((x + y) / 2)  ===  1/2

    describe "instance Eq" $ do        
        it "dirac x /= dirac y" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y  ==>  dirac x /= dirac y

    describe "elimination . introduction" $ do        
        it "unsafe fromMeasure . measure" $ property $
            \(m :: Prob Rational) ->
                m  ===  (unsafeFromMeasure . measure) m

        it "fromMeasure . measure" $ property $
            \(m :: Prob Rational) ->
                Just m  ===  (fromMeasure . measure) m

        it "unsafe fromDistribution . distribution" $ property $
            \(m :: Prob Rational) ->
                Just m  ===
                    (fmap unsafeFromMeasure . M.fromDistribution . distribution) m

        it "fromDistribution . distribution" $ property $
            \(m :: Prob Rational) ->
                Just m  ===
                    (fromDistribution . distribution) m

    describe "expectation" $ do
        it "unit" $ property $
            \(m :: Prob Rational) ->
                expectation (Poly.constant 1) m
                    === 1

        it "positivity" $ mapSize (`div` 2) $ property $
            \(m :: Prob Rational) (PositivePoly p) ->
                expectation p m
                    >=  0

    describe "moments" $ do
        it "mean is additive" $ mapSize (`div` 10) $ property $
            \(mx :: Prob Rational) my ->
                let mean' = mean . moments
                in  mean' (convolve mx my)
                        ===  mean' mx + mean' my

        it "variance is additive" $ mapSize (`div` 10) $ property $
            \(mx :: Prob Rational) my ->
                let variance' = variance . moments
                in  variance' (convolve mx my)
                        ===  variance' mx + variance' my

        it "skewness absorbs translate" $ property $
            \(m :: Prob Rational) y ->
                let skewness' = skewness . moments
                in  skewness' (translate y m)
                        === skewness' m

        it "kurtosis absorbs translate" $ property $
            \(m :: Prob Rational) y ->
                let kurtosis' = kurtosis . moments
                in  kurtosis' (translate y m)
                        === kurtosis' m

        it "kurtosis bounded below" $ property $
            \(m :: Prob Rational) ->
                let ms = moments m
                in  kurtosis ms
                        >=  (skewness ms)^(2 :: Int) + 1

    describe "choice" $ do
        it "distribution" $ property $
            \(Probability p) (mx :: Prob Rational) my z ->
                eval (distribution (choice p mx my)) z
                    === p * eval (distribution mx) z
                        + (1-p) * eval (distribution my) z

    describe "translate" $ do
        it "distribution" $ property $
            \(m :: Prob Rational) y x ->
                eval (distribution (translate y m)) x
                    ===  eval (distribution m) (x - y)

    describe "convolve" $ do
        it "dirac dirac" $ property $
            \(x :: Rational) y ->
                convolve (dirac x) (dirac y)
                    ===  dirac (x + y)

        it "dirac translate, left" $ property $ mapSize (`div` 10) $
            \(mx :: Prob Rational) (y :: Rational) ->
                convolve mx (dirac y)
                    ===  translate y mx

        it "dirac translate, right" $ property $ mapSize (`div` 10) $
            \(x :: Rational) (my :: Prob Rational) ->
                convolve (dirac x) my
                    ===  translate x my

        it "symmetric" $ property $ mapSize (`div` 10) $
            \mx (my :: Prob Rational) ->
                convolve mx my
                    ===  convolve my mx

        it "translate, left" $ property $ mapSize (`div` 10) $
            \mx (my :: Prob Rational) (Positive z) ->
                translate z (convolve mx my)
                    ===  convolve (translate z mx) my

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
newtype PositivePoly = PositivePoly (Poly.Poly Rational)
    deriving (Eq, Show)

instance Arbitrary PositivePoly where
    arbitrary = PositivePoly <$> genPositivePoly

newtype Probability = Probability Rational
    deriving (Eq, Show)

instance Arbitrary Probability where
    arbitrary = Probability <$> genProbability

instance Arbitrary (Prob Rational) where
    arbitrary = scale (`div` 15) genProb

-- | Generate a random 'Prob' by generating a random expression.
genProb :: Gen (Prob Rational)
genProb = do
    size <- getSize
    genProbFromList =<< vectorOf size genSimpleProb

-- | Generate a 'uniform'.
genUniform :: Gen (Prob Rational)
genUniform = do
    NonNegative a <- arbitrary
    Positive d <- arbitrary
    pure $ uniform a (a + d)

-- | Generate a 'dirac'.
genDirac :: Gen (Prob Rational)
genDirac = do
    NonNegative a <- arbitrary
    pure $ dirac a

-- | Generate a simple probability measure — one of 'uniform', 'dirac'.
genSimpleProb :: Gen (Prob Rational)
genSimpleProb =
    frequency [(20, genUniform), (4, genDirac)]

-- | Generate a random probability in the interval (0,1).
genProbability :: Gen Rational
genProbability = do
    denominator <- chooseInteger (1,2^(20 :: Int))
    numerator <- chooseInteger (0, denominator)
    pure (numerator % denominator)

-- | Generate a random 'Prob' by combining a given list
-- of 'Prob' with random operations.
genProbFromList :: [Prob Rational] -> Gen (Prob Rational)
genProbFromList [] = pure $ dirac 0
genProbFromList [x] = pure x
genProbFromList xs = do
    n <- choose (1, length xs - 1)
    let (ys, zs) = splitAt n xs
    genOp <*> genProbFromList ys <*> genProbFromList zs
  where
    genChoice = do
        p <- genProbability
        pure $ choice p
    genOp = oneof [pure convolve, genChoice]
