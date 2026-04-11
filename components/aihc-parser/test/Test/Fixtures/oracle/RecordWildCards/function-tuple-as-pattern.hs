{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
{- Test nested as-pattern in function pattern binding (not in do-block) -}
module FunctionTupleAsPattern where

data T = T { field :: Int }

f (a, b@T {..}) = undefined
