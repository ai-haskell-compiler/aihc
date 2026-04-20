{- ORACLE_TEST pass -}
{- Tests infix type operators after parenthesized expressions in constraints. -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
import Data.Type.Equality (type (==))

-- Parenthesized infix type expression followed by ~ in constraint
f1 :: (a == b) ~ 'True => a -> b
f1 = undefined

-- Same with double parens (the workaround that always worked)
f2 :: ((a == b) ~ 'True) => a -> b
f2 = undefined

-- Parenthesized type application followed by ~ in constraint
f3 :: (Maybe a) ~ Maybe b => a -> b
f3 = undefined

-- Type family application with parens followed by ~
type family F a
f4 :: (F a) ~ Bool => a -> a
f4 = undefined

-- In comma-separated constraint list with parenthesized infix item
f5 :: (Eq a, (a == b) ~ 'True) => a -> b
f5 = undefined
