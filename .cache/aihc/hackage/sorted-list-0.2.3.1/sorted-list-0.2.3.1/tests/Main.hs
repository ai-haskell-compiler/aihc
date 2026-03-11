
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (unless)
import Test.QuickCheck
  ( Testable, isSuccess, quickCheckResult
  , Arbitrary, arbitrary
    )
import System.Exit (exitFailure)
import Data.SortedList (SortedList)
import Data.SortedList qualified as SL
import Data.List qualified as List

-- | Test a property.
quickCheck :: Testable prop => String -> prop -> IO ()
quickCheck n p = do
  putStrLn $ "Testing property: " ++ n
  r <- quickCheckResult p
  unless (isSuccess r) exitFailure

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = SL.toSortedList <$> arbitrary

applyAsList :: Ord a => ([a] -> [a]) -> SortedList a -> SortedList a
applyAsList f = SL.toSortedList . f . SL.fromSortedList

main :: IO ()
main = do
  quickCheck "toSortedList . fromSortedList = id" $
    \xs -> applyAsList id xs == (xs :: SortedList Int)
  quickCheck "insert" $
    \x xs -> SL.insert x xs == applyAsList ((:) x) (xs :: SortedList Int)
  quickCheck "delete" $
    \x xs -> let ys :: SortedList Int
                 ys = SL.toSortedList $ x : xs
             in  SL.delete x ys == applyAsList (List.delete x) ys
  quickCheck "deleteAll" $
    \x xs -> SL.deleteAll (x :: Int) xs == SL.filter (/=x) xs
  quickCheck "elemOrd" $
    \x xs -> SL.elemOrd x xs == List.elem (x :: Int) (SL.fromSortedList xs)
  quickCheck "nub" $
    \xs -> SL.nub xs == applyAsList List.nub (xs :: SortedList Int)
