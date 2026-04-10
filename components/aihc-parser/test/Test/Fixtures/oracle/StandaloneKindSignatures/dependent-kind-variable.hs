{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneKindSignatures, PolyKinds, ExplicitForAll #-}

module DependentKindVariable where

type Foo :: forall {k}. k -> *
data Foo x = Foo
