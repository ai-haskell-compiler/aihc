{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module InfixConstructorUnboxedUnit where

data D = (# #) :. Int
