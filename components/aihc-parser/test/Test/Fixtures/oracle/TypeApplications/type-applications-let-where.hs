{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsLetWhere where

f :: a -> a
f x = x

g :: Int
g = let y = f @Int 1 in y

h :: Int
h = result
  where result = f @Int 2
