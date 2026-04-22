{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
module A where
data Pair a b = MkPair a b
pattern (:<) :: Pair a b -> [Pair a b] -> [Pair a b]
pattern x :< xs <- (x : xs)
  where
    (MkPair a b) :< xs = MkPair a b : xs
