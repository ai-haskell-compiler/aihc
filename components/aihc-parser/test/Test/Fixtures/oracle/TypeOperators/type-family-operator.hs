{- ORACLE_TEST xfail TypeFamilies with TypeOperators fails to parse operator as type family name -}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

-- Type families with operator names are not supported by the parser.

module TypeFamilyOperator where

type family (++) a b
