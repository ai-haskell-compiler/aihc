{- ORACLE_TEST pass -}
{-# LANGUAGE NondecreasingIndentation #-}

-- NondecreasingIndentation allows nested do blocks at the same indentation level,
-- including with nested lambdas.

module NondecreasingNestedDo where

f = g $ \x -> do
    g $ \y -> do
    action x y
  where
    g = undefined
    action = undefined
