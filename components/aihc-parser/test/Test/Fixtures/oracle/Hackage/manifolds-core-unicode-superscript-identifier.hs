{- ORACLE_TEST xfail reason="superscript digit identifiers are rejected in type constructor names" -}
{-# LANGUAGE UnicodeSyntax #-}

module M where

data S⁰_ r = PositiveHalfSphere
