{- ORACLE_TEST xfail transformers-base parenthesized empty-list instance head parsed as promotion -}
{-# LANGUAGE FlexibleInstances #-}
module ParenthesizedEmptyListInstance where

class C a

instance C ([])
