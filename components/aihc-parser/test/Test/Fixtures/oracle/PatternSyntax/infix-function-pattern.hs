{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module InfixFunctionPattern where

f :: T -> T -> T
x #|# _ = x
_ #|# y = y
