{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Partir
Copyright   :  (c) Oleksandr Zhabenko 2022-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com
-}
module Aftovolio.Partir where

import Aftovolio.Basis
import Aftovolio.DataG
import Data.Char (isDigit)
import qualified Data.Foldable as F
import Data.InsertLeft (InsertLeft (..))
import Data.List (filter, null, uncons)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Base
import GHC.Float
import GHC.Num
import GHC.Real
import Text.Read (readMaybe)

class (F.Foldable t) => ConstraintsG t a where
    decodeCDouble :: t a -> Double -> Bool

instance ConstraintsG [] Char where
    decodeCDouble xs !y
        | null xxs = True
        | t < '2' =
            (if t == '0' then (>) else (<))
                y
                (fromIntegral . fromMaybe 1 $ (readMaybe ts :: Maybe Integer))
        | otherwise = getScale c cs t y
      where
        xxs = filter isDigit xs
        (t, ts) = fromJust . uncons $ xxs
        (c, cs) = fromMaybe ('0', "1") . uncons $ ts
        getScale c0 ws t0 y0
            | c0 == '1' = (ords t0) (logBase 10 y0) base
            | c0 == '2' = (ords t0) (637.0 * atan y0) base -- atan Infinity * 637.0 \approx 1000.0
            | c0 == '3' = (ords t0) (sin (k * y0)) (0.01 * base1)
            | c0 == '4' = (ords t0) (cos (k * y0)) (0.01 * base1)
            | c0 == '5' = (ords t0) (sin (k * y0)) (0.001 * base2)
            | c0 == '6' = (ords t0) (cos (k * y0)) (0.001 * base2)
            | c0 == '7' = (ords t0) (sin (k * y0)) (-0.01 * base1)
            | c0 == '8' = (ords t0) (cos (k * y0)) (-0.01 * base1)
            | otherwise = (ords t0) (y0 ** k) base1
          where
            base = fromIntegral . fromMaybe 1 $ (readMaybe ws :: Maybe Integer)
            ords t0
                | t0 == '2' = (>)
                | otherwise = (<)
            (w, wws) = fromMaybe ('2', "") . uncons $ ws
            base1 = fromIntegral . fromMaybe 50 $ (readMaybe wws :: Maybe Integer)
            base2 = fromIntegral . fromMaybe 500 $ (readMaybe wws :: Maybe Integer)
            k = fromIntegral . fromMaybe 2 $ (readMaybe [w] :: Maybe Integer)

partitioningR ::
    ( InsertLeft t2 (Result [] Char b Double)
    , Monoid (t2 (Result [] Char b Double))
    , InsertLeft t2 Double
    , Monoid (t2 Double)
    ) =>
    String ->
    t2 (Result [] Char b Double) ->
    (t2 (Result [] Char b Double), t2 (Result [] Char b Double))
partitioningR !xs dataR
    | F.null dataR = (mempty, mempty)
    | otherwise = partiR (decodeCDouble xs) dataR
{-# INLINE partitioningR #-}
{-# SPECIALIZE partitioningR ::
    String ->
    [Result [] Char Double Double] ->
    ([Result [] Char Double Double], [Result [] Char Double Double])
    #-}

partitioningR2 ::
    ( InsertLeft t2 (Result2 a b Double)
    , Monoid (t2 (Result2 a b Double))
    , InsertLeft t2 Double
    , Monoid (t2 Double)
    ) =>
    String ->
    t2 (Result2 a b Double) ->
    (t2 (Result2 a b Double), t2 (Result2 a b Double))
partitioningR2 !xs dataR
    | F.null dataR = (mempty, mempty)
    | otherwise = partiR2 (decodeCDouble xs) dataR
{-# INLINE partitioningR2 #-}
{-# SPECIALIZE partitioningR2 ::
    (Eq a) =>
    String ->
    [Result2 a Double Double] ->
    ([Result2 a Double Double], [Result2 a Double Double])
    #-}
