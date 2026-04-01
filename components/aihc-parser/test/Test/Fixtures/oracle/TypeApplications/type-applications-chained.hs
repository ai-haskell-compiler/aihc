{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeApplicationsChained where

f :: a -> b -> a
f x _ = x

x :: Int
x = f @Int @Bool 1 True
