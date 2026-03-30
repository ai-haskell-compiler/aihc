{- ORACLE_TEST
id: binding
category: expressions
expected: xfail
reason: implicit parameter binding
-}
{-# LANGUAGE ImplicitParams #-}
module Binding where

f = ?x where ?x = 10
