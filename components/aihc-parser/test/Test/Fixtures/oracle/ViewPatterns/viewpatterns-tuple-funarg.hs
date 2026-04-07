{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleFunArg where

f (id -> x, y) = x
