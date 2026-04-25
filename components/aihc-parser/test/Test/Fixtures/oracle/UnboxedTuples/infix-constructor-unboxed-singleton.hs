{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module InfixConstructorUnboxedSingleton where

data D = (# a #) :. Int
