{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Calendar.BankHoliday.EnglandAndWalesSpec (spec) where

import Control.Monad
import Data.Time
import Test.Hspec
import Test.QuickCheck

import Data.Time.Calendar.BankHoliday.EnglandAndWales

spec :: Spec
spec = do
  describe "bankHolidays" $ do
    let inYear = map . uncurry . fromGregorian
    let hasBankHolidays yy dates = it ("works in " ++ show yy)
          $ bankHolidays yy `shouldBe` inYear yy dates

    1995 `hasBankHolidays` [(01,02), (04,14), (04,17), (05,08), (05,29), (08,28), (12,25), (12,26)]
    1999 `hasBankHolidays` [(01,01), (04,02), (04,05), (05,03), (05,31), (08,30), (12,27), (12,28), (12,31)]
    2002 `hasBankHolidays` [(01,01), (03,29), (04,01), (05,06), (06,03), (06,04), (08,26), (12,25), (12,26)]
    2011 `hasBankHolidays` [(01,03), (04,22), (04,25), (04,29), (05,02), (05,30), (08,29), (12,26), (12,27)]
    2012 `hasBankHolidays` [(01,02), (04,06), (04,09), (05,07), (06,04), (06,05), (08,27), (12,25), (12,26)]
    2013 `hasBankHolidays` [(01,01), (03,29), (04,01), (05,06), (05,27), (08,26), (12,25), (12,26)]
    2014 `hasBankHolidays` [(01,01), (04,18), (04,21), (05,05), (05,26), (08,25), (12,25), (12,26)]
    2015 `hasBankHolidays` [(01,01), (04,03), (04,06), (05,04), (05,25), (08,31), (12,25), (12,28)]
    2016 `hasBankHolidays` [(01,01), (03,25), (03,28), (05,02), (05,30), (08,29), (12,26), (12,27)]
    2020 `hasBankHolidays` [(01,01), (04,10), (04,13), (05,08), (05,25), (08,31), (12,25), (12,28)]
    2022 `hasBankHolidays` [(01,03), (04,15), (04,18), (05,02), (06,02), (06,03), (08,29), (09,19), (12,26), (12,27)]
    2023 `hasBankHolidays` [(01,02), (04,07), (04,10), (05,01), (05,08), (05,29), (08,28), (12,25), (12,26)]
    2024 `hasBankHolidays` [(01,01), (03,29), (04,01), (05,06), (05,27), (08,26), (12,25), (12,26)]

    it "always returns weekdays" $ property
      $ \yr -> not $ any (\d -> toModifiedJulianDay d `mod` 7 `elem` [3,4])
                $ bankHolidays yr

    it "always returns days in strictly increasing order" $ property
      $ \yr -> let isStrictlyIncreasing [] = True
                   isStrictlyIncreasing [_] = True
                   isStrictlyIncreasing (d1:ds@(d2:_)) = d1 < d2 && isStrictlyIncreasing ds
                in isStrictlyIncreasing $ bankHolidays yr

  describe "isBankHoliday" $
    forM_ [1990..2030] $ \yr ->
      it ("agrees with bankHolidays in " ++ show yr) $
        filter isBankHoliday [fromGregorian yr 01 01 .. fromGregorian yr 12 31]
        `shouldBe` bankHolidays yr

  describe "countBankHolidays" $ do
    it "counts the bank holidays between dates" $ property
      $ \(TestDay d0) (TestDay d1) ->
      countBankHolidays d0 d1 == countBankHolidaysNaive d0 d1

    it "satisfies 'countBankHolidays d0 d1 == negate (countBankHolidays d1 d0)'" $ property
      $ \(TestDay d0) (TestDay d1) ->
      countBankHolidays d0 d1 == negate (countBankHolidays d1 d0)

    it "satisfies 'countBankHolidays d0 d2 == countBankHolidays d0 d1 + countBankHolidays d1 d2'" $ property
      $ \(TestDay d0) (TestDay d1) (TestDay d2) ->
      countBankHolidays d0 d2 == countBankHolidays d0 d1 + countBankHolidays d1 d2

newtype TestDay = TestDay Day deriving (Show, Eq)

instance Arbitrary TestDay where
  arbitrary = TestDay <$> ModifiedJulianDay <$> choose
    ( toModifiedJulianDay $ fromGregorian 1990 01 01
    , toModifiedJulianDay $ fromGregorian 2030 01 01 )

countBankHolidaysNaive :: Day -> Day -> Integer
countBankHolidaysNaive d0 d1
  | d0 <= d1  = fromIntegral $ length $ filter isBankHoliday $ takeWhile (<d1) [d0..]
  | otherwise = negate $ countBankHolidaysNaive d1 d0
