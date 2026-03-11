{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Utility where

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import qualified Data.Char as Char
import System.Random (Random, )
import Control.Monad (liftM, )


newtype ArbChar = ArbChar Char
   deriving (Eq, Ord, Enum, Random)

instance Show ArbChar where
   showsPrec n (ArbChar c) = showsPrec n c

instance Arbitrary ArbChar where
   arbitrary = liftM (ArbChar . Char.chr . (32+) . flip mod 96) arbitrary
   shrink (ArbChar c) = map ArbChar $ shrink c

toLower :: ArbChar -> ArbChar
toLower (ArbChar c) = ArbChar (Char.toLower c)

toUpper :: ArbChar -> ArbChar
toUpper (ArbChar c) = ArbChar (Char.toUpper c)



type TimeDiff = NonNeg.Integer

timeToDouble :: TimeDiff -> NonNeg.Double
timeToDouble = fromIntegral

makeFracTime :: (TimeDiff, TimeDiff) -> NonNeg.Double
makeFracTime (n,d) =
   timeToDouble n / (timeToDouble d + 1)

intTimeList :: events TimeDiff body -> events TimeDiff body
intTimeList = id
