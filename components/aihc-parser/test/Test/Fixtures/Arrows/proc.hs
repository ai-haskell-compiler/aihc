{- ORACLE_TEST
id: proc
category: expressions
expected: xfail
reason: basic proc expression
-}
{-# LANGUAGE Arrows #-}
module Proc where

f g = proc x -> g -< x
