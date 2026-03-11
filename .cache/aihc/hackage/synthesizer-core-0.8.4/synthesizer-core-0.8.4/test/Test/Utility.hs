{-# LANGUAGE NoImplicitPrelude #-}
module Test.Utility where

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(arbitrary))

import qualified Number.Complex as Complex

import qualified Algebra.RealRing              as RealRing

import qualified Data.List.HT as ListHT
import qualified Data.Char as Char

import NumericPrelude.Base
import NumericPrelude.Numeric


approxEqual :: (RealRing.C a) => a -> a -> a -> Bool
approxEqual eps x y =
   2 * abs (x-y) <= eps * (abs x + abs y)

approxEqualAbs :: (RealRing.C a) => a -> a -> a -> Bool
approxEqualAbs eps x y =
   abs (x-y) <= eps

approxEqualListRel :: (RealRing.C a) => a -> [a] -> Bool
approxEqualListRel eps xs =
   let n = fromIntegral $ length xs
   in  approxEqualListAbs (eps * n * sum (map abs xs)) xs

approxEqualListAbs :: (RealRing.C a) => a -> [a] -> Bool
approxEqualListAbs eps xs =
   let n = fromIntegral $ length xs
       s = sum xs
   in  sum (map (\x -> abs (n*x-s)) xs)  <=  eps


approxEqualComplex ::
   (RealRing.C a) =>
   a -> Complex.T a -> Complex.T a -> Bool
approxEqualComplex eps x y =
   2 * Complex.magnitudeSqr (x-y)
      <= eps^2 * (Complex.magnitudeSqr x + Complex.magnitudeSqr y)

approxEqualComplexAbs ::
   (RealRing.C a) =>
   a -> Complex.T a -> Complex.T a -> Bool
approxEqualComplexAbs eps x y =
   Complex.magnitudeSqr (x-y) <= eps^2


-- see event-list

newtype ArbChar = ArbChar Char
   deriving (Eq, Ord)

instance Show ArbChar where
   showsPrec n (ArbChar c) = showsPrec n c

instance Arbitrary ArbChar where
   arbitrary = fmap (ArbChar . Char.chr) (QC.choose (32,127))

unpackArbString :: [ArbChar] -> String
unpackArbString =
   map (\(ArbChar c) -> c)
