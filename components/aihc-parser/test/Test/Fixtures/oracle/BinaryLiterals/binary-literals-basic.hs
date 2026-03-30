{- ORACLE_TEST pass -}
{-# LANGUAGE BinaryLiterals #-}

module BinaryLiteralsBasic where

maskA :: Int
maskA = 0b0001

maskB :: Int
maskB = 0B1010

combined :: Int
combined = maskA + maskB + 0b1111