{- ORACLE_TEST
id: promoted-ctor
category: types
expected: pass
-}
{-# LANGUAGE DataKinds #-}
module PromotedCtor where

data Nat = Zero | Succ Nat
type T = 'Zero
