{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
module A where
pattern x `Cons` xs <- (x : xs)
  where
    x `Cons` xs = x : xs
