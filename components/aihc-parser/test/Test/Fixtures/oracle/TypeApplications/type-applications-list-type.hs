{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsListType where

f :: a -> a
f x = x

x :: [Int]
x = f @[Int] [1, 2, 3]
