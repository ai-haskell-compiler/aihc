{- ORACLE_TEST
id: named-wildcard-forall
category: types
expected: pass
-}
{-# LANGUAGE NamedWildCards, ExplicitForAll #-}

module NamedWildcardForall where

poly :: forall _a. _a -> _a
poly x = x
