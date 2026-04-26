{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearLetBinding where

h :: Int -> Int
h x = let %1 y = x in y
