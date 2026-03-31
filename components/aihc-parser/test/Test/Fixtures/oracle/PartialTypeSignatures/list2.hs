{- ORACLE_TEST xfail wildcard in list element type -}
{-# LANGUAGE PartialTypeSignatures #-}
module List2 where

list :: [_]
list = [1]
