{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.CollectErrors.PreludeInstances where

import Prelude

import Control.CollectErrors ( CollectErrors(CollectErrors), lift, lift2 )
import Control.CollectErrors.PreludeInstances ( liftGotValue )
import Numeric.CollectErrors.Type

instance (Fractional v, Eq v) => Fractional (CN v) where
  fromRational = pure . fromRational
  recip = liftAcheck (==0) (\_ -> DivByZero) recip
  (/) = liftA2checkB (==0) (\_ -> DivByZero) (/)

instance (Integral v, Ord v, Show v) => Integral (CN v) where
  quotRem (CollectErrors (Just a) ae) (CollectErrors (Just b) be) 
    | b <= 0 = (e,e)
    | otherwise = (CollectErrors (Just q) es, CollectErrors (Just r) es)
    where
    (q,r) = quotRem a b
    es = ae <> be
    e = noValueNumErrorCertain (OutOfDomain $ "quotRem with non-positive denominator " ++ show b)
  quotRem (CollectErrors _ ae) (CollectErrors _ be) = (e,e)
    where
    e = CollectErrors Nothing (ae <> be)
  toInteger = liftGotValue "toInteger" toInteger

instance (Floating v, Ord v, Show v) => Floating (CN v) where
  pi = pure pi
  exp = lift exp
  log = liftAcheckPositive "log" log
  sqrt = liftAcheckNonnegative "sqrt" sqrt
  (**) = lift2 (**) -- TODO: domain check
  logBase = lift2 logBase -- TODO: domain check
  sin = lift sin
  cos = lift cos
  asin = liftAcheckPlusMinusOne "asin" asin
  acos = liftAcheckPlusMinusOne "acos" acos
  atan = lift atan
  sinh = lift sinh
  cosh = lift cosh
  asinh = lift asinh
  acosh = lift acosh
  atanh = lift atanh

liftAcheck :: 
  (a -> Bool) -> 
  (a -> NumError) -> 
  (a -> v) -> CN a -> CN v
liftAcheck check err _op (CollectErrors (Just a) _)
  | check a = noValueNumErrorCertain (err a)
liftAcheck _ _ op aCN = lift op aCN

liftAcheckPositive :: (Ord a, Num a, Show a) => String -> (a -> v) -> CN a -> CN v
liftAcheckPositive fnName =
  liftAcheck (<=0) (\x -> OutOfDomain $ fnName ++ " for non-positive arg " ++ show x)

liftAcheckNonnegative :: (Ord a, Num a, Show a) => String -> (a -> v) -> CN a -> CN v
liftAcheckNonnegative fnName =
  liftAcheck (<0) (\x -> OutOfDomain $ fnName ++ " for negative arg " ++ show x)

liftAcheckPlusMinusOne :: (Ord a, Num a, Show a) => String -> (a -> v) -> CN a -> CN v
liftAcheckPlusMinusOne fnName =
  liftAcheck (\x -> -1 <= x && x <= 1) (\x -> OutOfDomain $ fnName ++ " for illegal arg " ++ show x)

liftA2checkB :: 
  (b -> Bool) -> 
  (b -> NumError) -> 
  (a -> b -> v) -> 
  CN a -> CN b -> CN v
liftA2checkB checkB errB _op _a (CollectErrors (Just b) _)
  | checkB b = noValueNumErrorCertain (errB b)
liftA2checkB _ _ op a bCN = lift2 op a bCN
