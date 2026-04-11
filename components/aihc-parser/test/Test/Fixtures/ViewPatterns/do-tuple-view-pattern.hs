{- ORACLE_TEST pass -}
{- Test nested view pattern in tuple inside do-block -}
module DoTupleViewPattern where

f mx = do
  (a, b -> c) <- mx
  return undefined
