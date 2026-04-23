{- ORACLE_TEST pass -}
{-# LANGUAGE PolyKinds, TypeApplications, ExplicitForAll #-}

module ParenKindForall where

import Data.Proxy

class C a where c :: proxy a -> Integer

f :: forall k (a :: k). C a => Integer
f = c (Proxy @a)
