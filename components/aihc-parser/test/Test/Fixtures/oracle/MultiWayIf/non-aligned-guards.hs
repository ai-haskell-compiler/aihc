{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module NonAlignedGuards where

f :: Bool -> Bool -> Int
f x y = if | x -> 1
            | y -> 2
              | otherwise -> 3
