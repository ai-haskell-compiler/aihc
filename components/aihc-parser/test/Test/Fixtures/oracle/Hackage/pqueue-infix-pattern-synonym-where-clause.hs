{- ORACLE_TEST xfail infix explicitly-bidirectional pattern synonym where clause not parsed -}
{-# LANGUAGE PatternSynonyms #-}
module A where
pattern (:<) :: (a, b) -> [(a, b)] -> [(a, b)]
pattern x :< xs <- (x : xs)
  where
    (a, b) :< xs = (a, b) : xs
