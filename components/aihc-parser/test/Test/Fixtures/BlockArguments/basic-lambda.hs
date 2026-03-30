{- ORACLE_TEST
id: basic-lambda
category: expressions
expected: xfail
reason: basic lambda block argument
-}
{-# LANGUAGE BlockArguments #-}
module BasicLambda where

f = id \x -> x
