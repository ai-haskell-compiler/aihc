{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module InfixAppendListPattern where

data Infinite a = a :~ Infinite a

append :: [a] -> Infinite a -> Infinite a
[] `append` ys = ys
(x : xs) `append` ys = x :~ (xs `append` ys)
