{- ORACLE_TEST xfail type signature followed by guarded definition in where clause not supported -}
module AgdaWhereGuardedTypeSig where

f x = y
  where
    y :: Int
      | x > 0     = 1
      | otherwise = 0
