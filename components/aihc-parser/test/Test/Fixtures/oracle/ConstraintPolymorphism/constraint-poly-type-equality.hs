{- ORACLE_TEST pass -}
{- Tests constraint polymorphism with type equality constraints using (~). -}
{-# LANGUAGE GHC2021 #-}
module ConstraintPolyTypeEquality where

import Data.Proxy (Proxy)

-- Zero-arity constraint in parenthesized constrained type
f1 :: (() ~ () => a) -> a
f1 x = x

-- Zero-arity constraint without outer parens
f2 :: () ~ () => a -> a
f2 x = x

-- Multiple type equalities
f3 :: (a ~ b, b ~ c) => a -> c
f3 x = x

-- Type equality with complex types
f4 :: (Maybe a ~ Maybe b) => a -> b
f4 x = x

-- Type equality mixed with class constraints
f5 :: (Eq a, a ~ b) => b -> Bool
f5 x = x == x

-- Nested constrained type
f6 :: ((a ~ b) => a) -> b
f6 x = x

-- Type equality with Proxy
f7 :: Proxy a ~ Proxy b => a -> b
f7 x = x
