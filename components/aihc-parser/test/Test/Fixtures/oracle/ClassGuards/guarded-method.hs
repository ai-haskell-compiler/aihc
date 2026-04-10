{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

class C a where
  f :: a -> a -> a
  f x y | x == y = x
        | otherwise = y
