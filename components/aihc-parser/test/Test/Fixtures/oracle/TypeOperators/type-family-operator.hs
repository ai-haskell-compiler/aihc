{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

-- Type families with operator names are supported.

module TypeFamilyOperator where

type family (++) a b
