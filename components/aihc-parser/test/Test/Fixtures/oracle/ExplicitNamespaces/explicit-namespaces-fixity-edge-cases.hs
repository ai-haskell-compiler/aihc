{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityEdgeCases where

-- Test fixity with type namespace and precedence
infixl 7 type ***

-- Test fixity with data namespace and no precedence
infix data :+:

-- Test fixity with type namespace and precedence 0
infixr 0 type ==>

-- Test regular fixity without namespace (should still work)
infix 5 +++

-- Test multiple operators with data namespace
infixl 3 data @@+

type a *** b = (a, b)
type a ==> b = Either a b

data a :+: b = Left a | Right b
data a @@+ b = Plus a b
