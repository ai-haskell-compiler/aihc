{- ORACLE_TEST xfail explicit namespace fixity declarations are not yet parsed -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityDataOperator where

-- GHC 9.10 accepts this syntax for data operators, but aihc-parser does not yet.
infixr 0 data $

data a $ b = Dollar a b
