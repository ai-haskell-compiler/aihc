{- ORACLE_TEST xfail reason="view patterns inside record field patterns are rejected" -}
{-# LANGUAGE ViewPatterns #-}

module M where

data Box = Box {field :: Int}

f (Box {field = id -> x}) = x
