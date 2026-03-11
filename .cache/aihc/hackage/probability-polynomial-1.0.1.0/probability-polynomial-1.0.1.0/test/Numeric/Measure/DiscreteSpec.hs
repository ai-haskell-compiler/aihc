{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
-}
module Numeric.Measure.DiscreteSpec
    ( spec
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Numeric.Measure.Discrete
    ( Discrete
    , add
    , after
    , beforeOrAt
    , convolve
    , dirac
    , distribution
    , fromMap
    , integrate
    , scale
    , toMap
    , total
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
    , Positive (..)
    , (===)
    , (==>)
    , arbitrary
    , cover
    , property
    )

import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "instance Eq" $ do
        it "add m (scale (-1) m) == zero" $ property $
            \(m :: Discrete Rational) ->
                cover 80 (total m /= 0) "nontrivial"
                $ add m (scale (-1) m)  ===  zero

        it "dirac x /= dirac y" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y  ==>  dirac x /= dirac y

    describe "distribution" $ do
        it "eval and total" $ property $
            \(m :: Discrete Rational) ->
                let xlast = maybe 0 fst $ Map.lookupMax $ toMap m
                in  total m
                        === eval (distribution m) xlast

        it "eval and scale" $ property $
            \(m :: Discrete Rational) x s->
                eval (distribution (scale s m)) x
                    === s * eval (distribution m) x

    describe "integrate" $ do
        it "total" $ property $
            \(m :: Discrete Rational) ->
                integrate (const 1) m
                    === total m

        it "linearity, function (+)" $ property $
            \(mx :: Discrete Rational) ->
                let f = id
                in  integrate (\x -> f x + f x) mx
                        === integrate f mx + integrate f mx 

        it "linearity, measure add" $ property $
            \(mx :: Discrete Rational) my ->
                let f = id
                in  integrate f (add mx my)
                        === integrate f mx + integrate f my 

        it "linearity, measure scale" $ property $
            \(mx :: Discrete Rational) a ->
                let f = id
                in  integrate f (scale a mx)
                        === a * integrate f mx

    describe "translate" $ do
        it "distribution" $ property $
            \(m :: Discrete Rational) y x ->
                eval (distribution (translate y m)) x
                    ===  eval (distribution m) (x - y)

    describe "beforeOrAt and after" $ do
        it "add beforeOrAt after" $ property $
            \(mx :: Discrete Rational) t ->
                add (beforeOrAt t mx) (after t mx)  ===  mx

        it "eval distribution after" $ property $
            \(mx :: Discrete Rational) t ->
                eval (distribution (after t mx)) t  ===  0

    describe "convolve" $ do
        it "dirac" $ property $
            \(x :: Rational) y ->
                convolve (dirac x) (dirac y)
                    ===  dirac (x + y)

        it "total" $ property $
            \mx (my :: Discrete Rational) ->
                total (convolve mx my)
                    === total mx * total my

        it "symmetric" $ property $
            \mx (my :: Discrete Rational) ->
                convolve mx my
                    ===  convolve my mx

        it "distributive, left" $ property $
            \mx my (mz :: Discrete Rational) ->
                convolve (add mx my) mz
                    === add (convolve mx mz) (convolve my mz) 

        it "distributive, right" $ property $
            \mx my (mz :: Discrete Rational) ->
                convolve mx (add my mz)
                    === add (convolve mx my) (convolve mx mz) 

        it "translate, left" $ property $
            \mx (my :: Discrete Rational) (Positive z) ->
                translate z (convolve mx my)
                    ===  convolve (translate z mx) my

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance (Ord a, Num a, Arbitrary a) => Arbitrary (Discrete a) where
    arbitrary = fromMap . Map.fromList <$> arbitrary
