{- ORACLE_TEST
id: basic
category: expressions
expected: xfail
reason: required type argument in expression
-}
{-# LANGUAGE RequiredTypeArguments #-}
module Basic where

x = f (type Int) 5
