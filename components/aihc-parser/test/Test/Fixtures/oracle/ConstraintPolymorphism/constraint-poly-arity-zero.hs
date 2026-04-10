{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

f :: (() ~ () => a) -> a
f x = x
