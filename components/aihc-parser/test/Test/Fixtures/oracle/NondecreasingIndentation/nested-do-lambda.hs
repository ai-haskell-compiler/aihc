{- ORACLE_TEST xfail NondecreasingIndentation with nested do blocks in lambda fails to parse -}
{-# LANGUAGE NondecreasingIndentation #-}

-- NondecreasingIndentation allows nested do blocks at the same indentation level,
-- but the parser fails to handle this correctly with nested lambdas.

module NondecreasingNestedDo where

f = g $ \x -> do
    g $ \y -> do
    action x y
  where
    g = undefined
    action = undefined
