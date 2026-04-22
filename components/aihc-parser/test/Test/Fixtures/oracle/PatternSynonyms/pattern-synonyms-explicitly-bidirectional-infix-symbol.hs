{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
module A where
pattern (:<) :: a -> [a] -> [a]
pattern x :< xs <- (x : xs)
  where
    x :< xs = x : xs
