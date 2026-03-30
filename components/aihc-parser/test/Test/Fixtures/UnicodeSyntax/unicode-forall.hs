{- ORACLE_TEST
id: unicode-forall
category: types
expected: pass
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitForAll #-}

module UnicodeSyntaxForall where

identity ∷ ∀ a . a → a
identity x = x
