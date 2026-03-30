{- ORACLE_TEST
id: basic-case
category: expressions
expected: xfail
reason: basic case block argument
-}
{-# LANGUAGE BlockArguments #-}
module BasicCase where

f x = id case x of
  _ -> ()
