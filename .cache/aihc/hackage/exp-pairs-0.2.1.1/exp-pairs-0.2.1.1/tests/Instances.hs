{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances (Ratio01 (..), Positive (..), Sorted(..)) where

import Test.QuickCheck (Arbitrary(..), Gen, genericShrink, suchThat, vectorOf)
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad
import Data.List (sort)
import Data.Ratio
import GHC.Generics          (Generic (..))

import Math.ExpPairs.LinearForm
import Math.ExpPairs.Process
import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.Pair (InitPair' (..))
import Math.ExpPairs.Matrix3 as M3 (Matrix3, fromList)

instance Arbitrary a => Arbitrary (LinearForm a) where
  arbitrary = LinearForm <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (LinearForm a) where
  series = cons3 LinearForm

instance Arbitrary a => Arbitrary (RationalForm a) where
  arbitrary = (:/:) <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (RationalForm a) where
  series = cons2 (:/:)

instance Arbitrary a => Arbitrary (Constraint a) where
  arbitrary = Constraint <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (Constraint a) where
  series = cons2 Constraint

instance Arbitrary IneqType where
  arbitrary = f <$> arbitrary where
    f x = if x then Strict else NonStrict
  shrink = genericShrink

instance Monad m => Serial m IneqType where
  series = cons0 Strict \/ cons0 NonStrict

instance Arbitrary Process where
  arbitrary = f <$> arbitrary where
    f x = if x then A else BA
  shrink = genericShrink

instance Monad m => Serial m Process where
  series = cons0 A \/ cons0 BA

-- | Does not have a 'Num' instance!
newtype Ratio01 t = Ratio01 t
  deriving (Eq, Ord, Generic)

instance (Ord t, Integral t, Arbitrary t) => Arbitrary (Ratio01 (Ratio t)) where
  arbitrary = do
    denom <- arbitrary `suchThat` (> 0)
    numer <- arbitrary
    pure $ Ratio01 $ (numer `mod` (denom + 1)) % denom
  shrink (Ratio01 y) = Ratio01 <$> filter (\x -> 0 <= x && x <= 1) (shrink y)

instance (Ord t, Integral t, Serial m t) => Serial m (Ratio01 (Ratio t)) where
  series = Ratio01 <$> (series `suchThatSerial` (\x -> 0 <= x && x <= 1))

instance Show t => Show (Ratio01 t) where
  showsPrec n (Ratio01 x) = showsPrec n x

instance (Ord t, Integral t, Arbitrary t) => Arbitrary (InitPair' (Ratio t)) where
  arbitrary = f <$> liftM2 (,) arbitrary arbitrary where
    f :: (Ord t, Fractional t) => (Ratio01 t, Ratio01 t) -> InitPair' t
    f (Ratio01 x, Ratio01 y)
      | 100*x<5   = Corput01
      | 100*x<10  = Corput12
      | otherwise = Mix x' y' where
        x' = x*10/9
        y' = y*(1-x)
  shrink = genericShrink

instance (Ord t, Integral t, Serial m t) => Serial m (InitPair' (Ratio t)) where
  series = cons0 Corput01 \/ cons0 Corput12 \/ mseries
    where
      mseries = do
        (Ratio01 x) <- series
        (Ratio01 y) <- series
        return $ Mix x (y * (1-x))

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance (Arbitrary a) => Arbitrary (M3.Matrix3 a) where
  arbitrary = M3.fromList <$> vectorOf 9 arbitrary
  shrink = genericShrink

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty

newtype Sorted t = Sorted t
  deriving (Show, Generic)

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` uncurry (<=))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t)) where
  series = Sorted <$> (series `suchThatSerial` uncurry (<=))

instance (Num t, Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t)) where
  arbitrary = do
    a  <- arbitrary
    ab <- arbitrary `suchThat` (>= 0)
    bc <- arbitrary `suchThat` (>= 0)
    let b = a + ab; c = b + bc
    pure $ Sorted (a, b, c)

instance (Num t, Ord t, Serial m t) => Serial m (Sorted (t, t, t)) where
  series = do
    a  <- series
    ab <- series `suchThatSerial` (>= 0)
    bc <- series `suchThatSerial` (>= 0)
    let b = a + ab; c = b + bc
    pure $ Sorted (a, b, c)

instance (Num t, Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t)) where
  arbitrary = do
    a  <- arbitrary
    ab <- arbitrary `suchThat` (>= 0)
    bc <- arbitrary `suchThat` (>= 0)
    cd <- arbitrary `suchThat` (>= 0)
    let b = a + ab; c = b + bc; d = c + cd
    pure $ Sorted (a, b, c, d)
  shrink (Sorted (aa, bb, cc, dd))
    = map ((\[a, b, c, d] -> Sorted (a, b, c, d)) . sort)
    $ filter ((== 4) . length)
    $ shrink [aa, bb, cc, dd]

instance (Num t, Ord t, Serial m t) => Serial m (Sorted (t, t, t, t)) where
  series = do
    a  <- series
    ab <- series `suchThatSerial` (>= 0)
    bc <- series `suchThatSerial` (>= 0)
    cd <- series `suchThatSerial` (>= 0)
    let b = a + ab; c = b + bc; d = c + cd
    pure $ Sorted (a, b, c, d)

instance (Num t, Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t, t, t)) where
  arbitrary = do
    a  <- arbitrary
    ab <- arbitrary `suchThat` (>= 0)
    bc <- arbitrary `suchThat` (>= 0)
    cd <- arbitrary `suchThat` (>= 0)
    de <- arbitrary `suchThat` (>= 0)
    ef <- arbitrary `suchThat` (>= 0)
    let b = a + ab; c = b + bc; d = c + cd; e = d + de; f = e + ef
    pure $ Sorted (a, b, c, d, e, f)

instance (Num t, Ord t, Serial m t) => Serial m (Sorted (t, t, t, t, t, t)) where
  series = do
    a  <- series
    ab <- series `suchThatSerial` (>= 0)
    bc <- series `suchThatSerial` (>= 0)
    cd <- series `suchThatSerial` (>= 0)
    de <- series `suchThatSerial` (>= 0)
    ef <- series `suchThatSerial` (>= 0)
    let b = a + ab; c = b + bc; d = c + cd; e = d + de; f = e + ef
    pure $ Sorted (a, b, c, d, e, f)

instance (Num t, Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t, t, t, t, t)) where
  arbitrary = do
    a  <- arbitrary
    ab <- arbitrary `suchThat` (>= 0)
    bc <- arbitrary `suchThat` (>= 0)
    cd <- arbitrary `suchThat` (>= 0)
    de <- arbitrary `suchThat` (>= 0)
    ef <- arbitrary `suchThat` (>= 0)
    fg <- arbitrary `suchThat` (>= 0)
    gh <- arbitrary `suchThat` (>= 0)
    let b = a + ab; c = b + bc; d = c + cd; e = d + de; f = e + ef; g = f + fg; h = g + gh
    pure $ Sorted (a, b, c, d, e, f, g, h)
  shrink (Sorted (aa, bb, cc, dd, ee, ff, gg, hh))
    = map ((\[a, b, c, d, e, f, g, h] -> Sorted (a, b, c, d, e, f, g, h)) . sort)
    $ filter ((== 8) . length)
    $ shrink [aa, bb, cc, dd, ee, ff, gg, hh]

instance Arbitrary Path where
  arbitrary = foldMap (\x -> if x then aPath else baPath) <$> (arbitrary :: Gen [Bool])

instance Monad m => Serial m Path where
  series = foldMap (\x -> if x then aPath else baPath) <$> (series :: Monad m => Series m [Bool])
