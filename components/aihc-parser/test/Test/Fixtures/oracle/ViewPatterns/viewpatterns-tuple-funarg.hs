{- ORACLE_TEST xfail tuple argument with view pattern element -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleFunArg where

f (id -> x, y) = x
