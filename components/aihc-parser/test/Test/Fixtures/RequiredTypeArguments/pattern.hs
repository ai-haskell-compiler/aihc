{- ORACLE_TEST
id: parallel
category: patterns
expected: xfail
reason: required type argument in pattern
-}
{-# LANGUAGE RequiredTypeArguments #-}
module Pattern where

f (type a) x = x
