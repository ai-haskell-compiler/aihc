{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeOperators #-}

module PromotedTypeOperatorWithNames where

-- Promoted type operator with type variables
type T1 = a ':$$: b

-- Promoted type operator with type constructors
type T2 = Maybe ':$$: Either

-- Promoted type operator with concrete types
type T3 = Int ':$$: Bool
