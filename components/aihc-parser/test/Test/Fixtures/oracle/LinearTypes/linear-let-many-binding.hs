{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearLetManyBinding where

h :: Int -> Int
h x = let %Many y = x in y
