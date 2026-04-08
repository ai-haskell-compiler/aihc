{- ORACLE_TEST xfail explicit namespace fixity declarations are not yet parsed -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityTypeOperator where

-- GHC 9.10 accepts this syntax for type operators, but aihc-parser does not yet.
infixl 9 type $

type a $ b = Either a b
