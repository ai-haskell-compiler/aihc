{- ORACLE_TEST xfail do-block tuple pattern with as-pattern and record wildcards -}
{-# LANGUAGE RecordWildCards #-}
module DoTupleAsPattern where

data T = T { field :: Int }

f mx = do
  (a, b@T {..}) <- mx
  return undefined
