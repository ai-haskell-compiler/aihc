{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module GuardedWhereTuplePattern where

-- Parser fails to handle guarded patterns with tuple patterns in where clauses
f :: Int -> Int
f x = y
  where
    (a, b)
      | x <= 0 = (0, 1)
      | otherwise = (1, 0)
    y = a
