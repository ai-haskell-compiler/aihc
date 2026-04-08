{- ORACLE_TEST pass -}
{-# LANGUAGE FlexibleInstances #-}
module ParenthesizedEmptyListInstance where

class C a

instance C ([])
