{- ORACLE_TEST
id: forall-top-level
category: types
expected: pass
-}
{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllTopLevel where

identity :: forall a. a -> a
identity x = x
