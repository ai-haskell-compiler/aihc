{- ORACLE_TEST
id: data-instance-gadt
category: declarations
expected: xfail
reason: data instance with GADT syntax
-}
{-# LANGUAGE TypeFamilies, GADTs #-}
module DataInstanceGADT where

data family G a b
data instance G [a] b where
   G1 :: c -> G [Int] b
   G2 :: G [a] Bool
