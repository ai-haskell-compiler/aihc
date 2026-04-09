{- ORACLE_TEST xfail infix function pattern -}
{-# LANGUAGE GHC2021 #-}

module InfixFunctionPattern where

f :: T -> T -> T
x #|# _ = x
_ #|# y = y
