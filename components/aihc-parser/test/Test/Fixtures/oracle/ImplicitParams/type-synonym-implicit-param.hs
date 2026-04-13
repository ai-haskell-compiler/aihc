{- ORACLE_TEST xfail reason="implicit parameter syntax in type synonyms not recognized" -}
{-# LANGUAGE Haskell2010, ImplicitParams, ConstraintKinds #-}

-- Parser fails on implicit parameter syntax in type synonyms
type CanCheck = (?checker :: Int)

f :: CanCheck => Int
f = ?checker
