{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
module UnquotedListTypeEdgeCases where

-- Single element (should still work)
type Single = [Int]

-- Two elements
type Two = [Int, Bool]

-- Three elements
type Three = [Int, Bool, String]

-- Multiple elements with type variables
type WithVars = [a, b, c]

-- Nested list types
type Nested = [[Int, Bool], [Char]]

-- In function signature
type Func = [Int, Bool] -> [String, Int]

-- Promoted with quote (should still work)
type Promoted = '[Int, Bool, String]
