{- ORACLE_TEST
id: type-operator-type-synonym
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE TypeOperators #-}

module TypeOperatorTypeSynonym where

infixr 6 :*:
type a :*: b = (a, b)
