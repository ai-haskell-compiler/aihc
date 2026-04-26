{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearWhereBinding where

h :: Int -> Int
h x = y
  where
    %1 y = x
