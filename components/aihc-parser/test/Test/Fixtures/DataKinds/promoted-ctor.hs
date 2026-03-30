{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
module PromotedCtor where

data Nat = Zero | Succ Nat
type T = 'Zero