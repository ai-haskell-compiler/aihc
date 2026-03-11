{-# OPTIONS_GHC -Wno-orphans #-}
module Control.CollectErrors.PreludeInstances where

import Prelude

import Text.Printf ( printf )

import Control.CollectErrors.Type
    ( CollectErrors(CollectErrors), CanBeErrors, lift, lift2 )


instance (CanBeErrors es, Eq v) => Eq (CollectErrors es v) where
  (==) = liftGotValues2 "(==)" (==)

instance (CanBeErrors es, Ord v) => Ord (CollectErrors es v) where
  compare = liftGotValues2 "compare" compare
  (<) = liftGotValues2 "(<)" (<)
  (<=) = liftGotValues2 "(<=)" (<=)
  (>) = liftGotValues2 "(>)" (>)
  (>=) = liftGotValues2 "(>=)" (>=)
  max = lift2 max
  min = lift2 min

instance (CanBeErrors es, Bounded v) => Bounded (CollectErrors es v) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (CanBeErrors es, Enum v) => Enum (CollectErrors es v) where
  toEnum = pure . toEnum
  fromEnum = liftGotValue "fromEnum" fromEnum

instance (CanBeErrors es, Num v) => Num (CollectErrors es v) where
  fromInteger = pure . fromInteger
  (+) = lift2 (+)
  (-) = lift2 (-)
  (*) = lift2 (*)
  abs = lift abs
  negate = lift negate
  signum = lift signum

instance (CanBeErrors es, Real v) => Real (CollectErrors es v) where
  toRational = liftGotValue "toRational" toRational


{- Utilities -}

errorMissingValue :: (Show t, Monoid t) => String -> t -> t2
errorMissingValue label es = 
  error $ printf "Missing value in %s: %s" label (show es)

errorMissingValues :: (Show t, Monoid t) => String -> [t] -> t2
errorMissingValues label ess = 
  error $ printf "Missing value(s) in %s: %s" label (show $ mconcat ess)

liftGotValue :: (CanBeErrors es) => String -> (t1 -> t) -> CollectErrors es t1 -> t
liftGotValue _ op (CollectErrors (Just v1) _) = 
  op v1
liftGotValue label _op (CollectErrors _ es1) = 
  errorMissingValue label es1

liftGotValues2 :: (CanBeErrors es) => String -> (t1 -> t2 -> t) -> CollectErrors es t1 -> CollectErrors es t2 -> t
liftGotValues2 _ op (CollectErrors (Just v1) _) (CollectErrors (Just v2) _) = 
  op v1 v2
liftGotValues2 label _op (CollectErrors _ es1) (CollectErrors _ es2) = 
  errorMissingValues label [es1, es2]
