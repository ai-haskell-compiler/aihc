{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies, GADTs #-}
module DataInstanceGADT where

data family G a b
data instance G [a] b where
   G1 :: c -> G [Int] b
   G2 :: G [a] Bool