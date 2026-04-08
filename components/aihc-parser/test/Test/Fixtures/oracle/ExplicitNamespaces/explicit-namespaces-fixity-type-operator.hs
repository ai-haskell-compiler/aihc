{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityTypeOperator where

-- GHC 9.10 accepts this syntax for type operators with explicit namespace.
infixl 9 type $

type a $ b = Either a b
