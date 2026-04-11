{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
{- Test deeply nested as-pattern in tuple -}
module DoTupleAsPatternDeep where

data T = T { field :: Int }

f mx = do
  (a, (b, c@T {..}), d) <- mx
  return undefined
