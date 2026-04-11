{- ORACLE_TEST pass -}
{- Test nested as-pattern in tuple (simple constructor application) -}
module DoTupleAsPatternSimple where

data T a = T a

f mx = do
  (a, b@(T x)) <- mx
  return undefined
