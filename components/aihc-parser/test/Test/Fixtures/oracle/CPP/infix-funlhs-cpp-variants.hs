{- ORACLE_TEST pass -}
{- Test infix function definitions with C preprocessor -}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module InfixFunlhsCppVariants where

-- Basic infix
infixl 6 <.>
(<.>) :: Int -> Int -> Int
x <.> y = x * y

-- Infix with constructor patterns
infixr 5 <++>
(<++>) :: Maybe a -> Maybe a -> Maybe a
(Just x) <++> _ = Just x
Nothing <++> y = y

#ifdef DEBUG
-- Infix inside CPP conditional
infix 4 <=>
(<=>) :: Int -> Int -> Ordering
x <=> y = compare x y
#endif
