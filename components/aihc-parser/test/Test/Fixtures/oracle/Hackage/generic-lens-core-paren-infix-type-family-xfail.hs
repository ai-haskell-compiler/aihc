{- ORACLE_TEST xfail parser rejects parenthesised infix type family head -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module GenericLensCoreParenInfixTypeFamily where

type family (a ++ b)
