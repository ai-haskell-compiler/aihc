{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module M where

data Box = Box {field :: Int}

f (Box {field = id -> x}) = x
