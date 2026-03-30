{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitForAll #-}

module UnicodeSyntaxForall where

identity ∷ ∀ a . a → a
identity x = x