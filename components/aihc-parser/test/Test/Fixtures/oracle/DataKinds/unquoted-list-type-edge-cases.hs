{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
module UnquotedListTypeEdgeCases where

-- Single element (should still work)
type Single = [Int]

-- Two elements
type Two = [Int, Bool]

-- Three elements
type Three = [Int, Bool, String]

-- Multiple elements with type variables (need forall to bind them)
type WithVars = forall a b c. [a, b, c]

-- Nested list types (all elements must have same kind [*])
type Nested = [[Int, Bool], [String, Bool]]

-- Promoted with quote (should still work)
type Promoted = '[Int, Bool, String]
