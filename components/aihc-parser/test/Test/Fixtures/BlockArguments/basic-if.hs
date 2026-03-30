{- ORACLE_TEST
id: basic-if
category: expressions
expected: xfail
reason: basic if block argument
-}
{-# LANGUAGE BlockArguments #-}
module BasicIf where

f x y z = id if x then y else z
