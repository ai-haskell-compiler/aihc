{- ORACLE_TEST
id: type-operator-data-infix
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE TypeOperators #-}

module TypeOperatorDataInfix where

infixr 5 :+:
data a :+: b = InL a | InR b
