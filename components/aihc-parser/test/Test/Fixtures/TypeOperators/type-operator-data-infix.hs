{- ORACLE_TEST xfail parser support pending -}
{-# LANGUAGE TypeOperators #-}

module TypeOperatorDataInfix where

infixr 5 :+:
data a :+: b = InL a | InR b