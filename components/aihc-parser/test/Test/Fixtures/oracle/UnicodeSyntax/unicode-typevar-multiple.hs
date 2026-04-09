{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}

-- Multiple unicode type variables in class declaration

module UnicodeTypeVarMultiple where

class Functor φ where
  fmap :: (α → β) → φ α → φ β
