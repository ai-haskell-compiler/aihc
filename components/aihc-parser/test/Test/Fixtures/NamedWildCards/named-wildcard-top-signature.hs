{- ORACLE_TEST
id: named-wildcard-top-signature
category: types
expected: pass
-}
{-# LANGUAGE NamedWildCards #-}

module NamedWildcardTopSignature where

identity :: _a -> _a
identity x = x
