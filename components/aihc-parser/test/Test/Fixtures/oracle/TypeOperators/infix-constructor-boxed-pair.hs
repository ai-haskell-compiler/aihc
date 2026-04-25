{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module InfixConstructorBoxedPair where

data D = (a, b) :. Int
