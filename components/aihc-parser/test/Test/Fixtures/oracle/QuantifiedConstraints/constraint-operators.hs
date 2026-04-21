{- ORACLE_TEST pass -}
{-# LANGUAGE QuantifiedConstraints #-}

module QuantifiedConstraintOperators where

class (p => q) => p |- q
instance (p => q) => p |- q
class (p,q) => p & q
instance (p,q) => p & q
