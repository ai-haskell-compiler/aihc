{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

-- Additional test cases for type families with operator names.

module TypeFamilyOperatorExtended where

-- Parenthesized variable operator (starts with non-:)
type family (++) a b

-- Parenthesized constructor operator (starts with :)
type family (:++:) a b

-- Type family with operator and kind signature
type family (<?>) a b :: Bool

-- Multiple type families with operators
type family (<>) a b
type family (><) a b c
