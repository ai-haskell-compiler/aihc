{- ORACLE_TEST xfail dependent kind variable {k} in forall -}
{-# LANGUAGE StandaloneKindSignatures, PolyKinds, ExplicitForAll #-}

module DependentKindVariable where

type Foo :: forall {k}. k -> *
data Foo x = Foo
