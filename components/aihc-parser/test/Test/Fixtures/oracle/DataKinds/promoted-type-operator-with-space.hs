{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module PromotedTypeOperatorWithSpace where

import GHC.TypeLits (TypeError, ErrorMessage(..), Symbol, Nat)

-- Regression test: the promotion tick before a type operator with intervening
-- whitespace (e.g. ' :<>:) was not recognized by the lexer, causing the
-- 'where' keyword in a closed type family to be mis-parsed as an unexpected
-- token.  This snippet parses with GHC but previously failed with aihc-parser.

type family ShowConstraint (s :: Symbol) (n :: Nat) :: Symbol where
  ShowConstraint s n = TypeError ('Text "value " ' :<>: 'ShowType n ' :<>: 'Text " exceeds limit for " ' :<>: 'ShowType s)

-- Multiple chained promoted operators with space before tick.
type family ConstraintMsg (s :: Symbol) (n :: Nat) :: Symbol where
  ConstraintMsg s n = TypeError ('Text "Invalid NonEmptyText. Needs to be <= " ' :<>: 'ShowType n ' :<>: 'Text " characters. Has " ' :<>: 'ShowType n ' :<>: 'Text " characters.")
