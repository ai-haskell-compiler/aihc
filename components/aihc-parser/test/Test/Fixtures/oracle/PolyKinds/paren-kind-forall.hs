{- ORACLE_TEST xfail parenthesized kind-annotated type variable in forall -}
{-# LANGUAGE PolyKinds, TypeApplications, AllowAmbiguousTypes, ExplicitForAll #-}

module ParenKindForall where

import Data.Proxy

class C a where c :: proxy a -> Integer

f :: forall k (a :: k). C a => Integer
f = c (Proxy @a)
