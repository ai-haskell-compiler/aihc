{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsExportPatternKeyword
  ( pattern Zero,
    pattern Succ,
  )
where

data Nat = Z | S Nat

pattern Zero :: Nat
pattern Zero = Z

pattern Succ :: Nat -> Nat
pattern Succ n = S n
