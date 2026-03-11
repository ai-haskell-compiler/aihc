module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Assertions
import Data.AEq
import Numeric.IEEE

check, check2 :: (Double -> Double -> Result) -> (Double -> Double -> Result) -> (Double -> Double -> Bool) -> Double -> Double -> Property
check ck1 ck2 pre a b = pre a b ==>
                         (ck1 a b) .&&. (ck2 a b)

check2 ck1 ck2 pre a b = (not $ pre a b) ==>
                          (expectFailure $ (ck1 a b .&&. ck2 a b))


main :: IO ()
main = hspec $ describe "QC asserts" $ do
  describe ">" $ do
    prop "must pass" $ check (?>) (>?) (>)
    prop "must fail" $ check2 (?>) (>?) (>)
  describe "<" $ do
    prop "must pass" $ check (?<) (<?) (<)
    prop "must fail" $ check2 (?<) (<?) (<)
  describe "==" $ do
    prop "must pass" $ \a -> check (==?) (?==) (==) a a
    prop "must fail" $ check2 (==?) (?==) (==)
  describe "/=" $ do
    prop "must pass" $ check (/=?) (?/=) (/=)
    prop "must fail" $ \a -> check2 (/=?) (?/=) (/=) a a
  describe ">=" $ do
    prop "must pass" $ check (>=?) (?>=) (>=)
    prop "must fail" $ check2 (>=?) (?>=) (>=)
  describe "<=" $ do
    prop "must pass" $ check (<=?) (?<=) (<=)
    prop "must fail" $ check2 (<=?) (?<=) (<=)
  describe "~==" $ do
    prop "must pass" $ \a -> check (?~==) (~==?) (~==) a (succIEEE a)
    prop "must pass" $ \a -> check (?~==) (~==?) (~==) a (predIEEE a)
    prop "must fail" $ check2 (?~==) (~==?) (~==)
