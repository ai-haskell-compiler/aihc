{- ORACLE_TEST xfail wildcard in list type -}
{-# LANGUAGE PartialTypeSignatures #-}
module List1 where

list :: _ Int
list = [1]
