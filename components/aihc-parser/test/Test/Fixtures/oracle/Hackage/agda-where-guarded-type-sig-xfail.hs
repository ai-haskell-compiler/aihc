{- ORACLE_TEST pass -}
module AgdaWhereGuardedTypeSig where

f x = y
  where
    y :: Int
      | x > 0     = 1
      | otherwise = 0
