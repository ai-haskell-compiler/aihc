{- ORACLE_TEST xfail parenthesized kind-annotated type variable in forall -}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module KindParenForall where

f :: forall k (a :: k). a
f = undefined
