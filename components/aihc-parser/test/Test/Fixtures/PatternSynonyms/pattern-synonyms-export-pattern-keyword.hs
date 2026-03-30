{- ORACLE_TEST
id: pattern-synonyms-export-pattern-keyword
category: modules
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsExportPatternKeyword
  ( pattern Zero,
    pattern Succ,
  ) where

data Nat = Z | S Nat

pattern Zero :: Nat
pattern Zero = Z

pattern Succ :: Nat -> Nat
pattern Succ n = S n
