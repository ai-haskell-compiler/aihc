{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010, ImplicitParams, ConstraintKinds #-}

-- Implicit parameter syntax in type synonyms
type CanCheck = (?checker :: Int)

f :: CanCheck => Int
f = ?checker
