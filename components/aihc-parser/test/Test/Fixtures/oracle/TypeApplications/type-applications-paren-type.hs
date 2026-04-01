{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsParenType where

f :: a -> a
f x = x

x :: Maybe Int
x = f @(Maybe Int) (Just 1)
