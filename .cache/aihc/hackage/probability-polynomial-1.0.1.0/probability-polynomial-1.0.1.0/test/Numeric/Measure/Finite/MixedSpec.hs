{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
-}
module Numeric.Measure.Finite.MixedSpec
    ( spec
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Data.Maybe
    ( fromJust
    )
import Numeric.Measure.Finite.Mixed
    ( Measure
    , add
    , after
    , beforeOrAt
    , convolve
    , dirac
    , distribution
    , fromDistribution
    , integrate
    , isPositive
    , scale
    , support
    , total
    , translate
    , uniform
    , zero
    )
import Numeric.Function.PiecewiseSpec
    ( genPiecewise
    )
import Numeric.Polynomial.SimpleSpec
    ( genPoly
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
    , (==>)
    , arbitrary
    , conjoin
    , counterexample
    , cover
    , mapSize
    , once
    , property
    )

import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "dirac" $ do
        it "total" $ property $
            \(x :: Rational) ->
                total (dirac x)  ===  1

    describe "uniform" $ do
        it "total" $ property $
            \(x :: Rational) y ->
                total (uniform x y)  ===  1

        it "support" $ property $
            \(x :: Rational) y ->
                support (uniform x y)  ===  Just (min x y, max x y)

        it "distribution at midpoint" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y ==>
                eval (distribution (uniform x y)) ((x + y) / 2)  ===  1/2

    describe "instance Eq" $ do
        it "add m (scale (-1) m) == zero" $ property $
            \(m :: Measure Rational) ->
                cover 80 (total m /= 0) "nontrivial"
                $ add m (scale (-1) m)  ===  zero
        
        it "dirac x /= dirac y" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y  ==>  dirac x /= dirac y

    describe "add" $ do
        it "total" $ property $
            \(mx :: Measure Rational) my ->
                total (add mx my)  ===  total mx + total my

    describe "translate" $ do
        it "distribution" $ property $
            \(m :: Measure Rational) y x ->
                eval (distribution (translate y m)) x
                    ===  eval (distribution m) (x - y)

    describe "convolve" $ do
        it "dirac dirac" $ property $
            \(x :: Rational) y ->
                convolve (dirac x) (dirac y)
                    ===  dirac (x + y)

        it "total" $ property $ mapSize (`div` 10) $
            \mx (my :: Measure Rational) ->
                total (convolve mx my)
                    ===  total mx * total my

        it "dirac translate, left" $ property $ mapSize (`div` 10) $
            \(mx :: Measure Rational) (y :: Rational) ->
                convolve mx (dirac y)
                    ===  translate y mx

        it "dirac translate, right" $ property $ mapSize (`div` 10) $
            \(x :: Rational) (my :: Measure Rational) ->
                convolve (dirac x) my
                    ===  translate x my

        it "symmetric" $ property $ mapSize (`div` 10) $
            \mx (my :: Measure Rational) ->
                convolve mx my
                    ===  convolve my mx

        it "distributive, left" $ property $ mapSize (`div` 12) $
            \mx my (mz :: Measure Rational) ->
                convolve (add mx my) mz
                    ===  add (convolve mx mz) (convolve my mz) 

        it "distributive, right" $ property $ mapSize (`div` 12) $
            \mx my (mz :: Measure Rational) ->
                convolve mx (add my mz)
                    ===  add (convolve mx my) (convolve mx mz) 

        it "translate, left" $ property $ mapSize (`div` 10) $
            \mx (my :: Measure Rational) (Positive z) ->
                translate z (convolve mx my)
                    ===  convolve (translate z mx) my

    describe "isPositive" $ do
        it "scale dirac" $ property $
            \(x :: Rational) w ->
                isPositive (scale w (dirac x))
                    ===  (w >= 0)

        it "sum of positive dirac" $ property $
            \(ws :: [Positive Rational]) ->
                let mkDirac i (Positive w) = scale w (dirac i)
                    diracs = zipWith mkDirac [1..] ws
                in  isPositive (foldr add zero diracs)
                        === True

        it "nfold convolution of uniform" $ once $
            let convolutions :: [Measure Rational]
                convolutions =
                    iterate (convolve (uniform 0 1)) (dirac 0)
                prop_isPositive m =
                    counterexample (show m)
                    $ isPositive m  ===  True
            in  conjoin
                    $ take 20
                    $ map prop_isPositive convolutions

    describe "integrate" $ do
        it "total" $ mapSize (`div` 10) $ property $
            \(m :: Measure Rational) ->
                integrate (Poly.constant 1) m
                    === total m

        it "linearity, function (+)" $ mapSize (`div` 10) $ property $
            \f g (mx :: Measure Rational) ->
                integrate (f + g) mx
                    === integrate f mx + integrate g mx 

        it "linearity, measure add" $ mapSize (`div` 10) $ property $
            \(mx :: Measure Rational) my ->
                let f = Poly.fromCoefficients [0,1]
                in  integrate f (add mx my)
                        === integrate f mx + integrate f my 

        it "linearity, measure scale" $ mapSize (`div` 10) $ property $
            \(mx :: Measure Rational) a ->
                let f = Poly.fromCoefficients [0,1]
                in  integrate f (scale a mx)
                        === a * integrate f mx

    describe "beforeOrAt and after" $ do
        it "add beforeOrAt after" $ property $
            \(mx :: Measure Rational) t ->
                add (beforeOrAt t mx) (after t mx)  ===  mx

        it "support after" $ property $
            \(mx :: Measure Rational) t ->
                let isAfterOrAt Nothing = True
                    isAfterOrAt (Just (x1, _)) = x1 >= t
                in  isAfterOrAt (support (after t mx))

        it "support beforeOrAt" $ property $
            \(mx :: Measure Rational) t ->
                let isBeforeOrAt Nothing = True
                    isBeforeOrAt (Just (_, x2)) = x2 <= t
                in  isBeforeOrAt (support (beforeOrAt t mx))

        it "eval distribution after" $ property $
            \(mx :: Measure Rational) t ->
                eval (distribution (after t mx)) t  ===  0

        it "after support" $ property $
            \(mx :: Measure Rational) ->
                case support mx of
                    Nothing -> property True
                    Just (_, x2) ->
                        cover 70 True "non-trivial" $
                            after x2 mx  ===  zero

        it "beforeOrAt support" $ property $
            \(mx :: Measure Rational) ->
                case support mx of
                    Nothing -> property True
                    Just (x1, _) ->
                        let value1 = eval (distribution mx) x1
                        in  cover 70 True "non-trivial" $
                                beforeOrAt x1 mx  ===  scale value1 (dirac x1)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
genMeasure :: Gen (Measure Rational)
genMeasure =
    fromJust . fromDistribution . setLastPieceConstant <$> genPiecewise genPoly
  where
    setLastPieceConstant =
        Piecewise.fromAscPieces
        . setLastPieceConstant'
        . Piecewise.toAscPieces

    setLastPieceConstant' [] = []
    setLastPieceConstant' [(x, o)] = [(x, Poly.constant (eval o x))]
    setLastPieceConstant' (p : ps) = p : setLastPieceConstant' ps

instance Arbitrary (Measure Rational) where
    arbitrary = genMeasure
