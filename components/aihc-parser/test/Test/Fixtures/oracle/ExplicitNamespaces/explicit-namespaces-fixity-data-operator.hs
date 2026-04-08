{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityDataOperator where

-- GHC 9.10 accepts this syntax for data operators with explicit namespace.
infixr 0 data $

data a $ b = Dollar a b
