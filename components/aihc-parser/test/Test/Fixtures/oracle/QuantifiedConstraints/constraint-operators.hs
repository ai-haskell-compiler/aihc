{- ORACLE_TEST xfail parser rejects multi-constraint class with (|-) and (&) operators -}
{-# LANGUAGE QuantifiedConstraints #-}

module QuantifiedConstraintOperators where

class (p => q) => p |- q
instance (p => q) => p |- q
class (p,q) => p & q
instance (p,q) => p & q
