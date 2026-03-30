{- ORACLE_TEST
id: pattern-synonyms-explicitly-bidirectional
category: patterns
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsExplicitlyBidirectional where

pattern NonEmpty :: a -> [a] -> [a]
pattern NonEmpty x xs <- (x : xs)
  where
    NonEmpty x xs = x : xs

headOr :: a -> [a] -> a
headOr d (NonEmpty x _) = x
headOr d [] = d
