{- ORACLE_TEST
id: named-puns-pattern
category: expressions
expected: pass
reason: parser now supports NamedFieldPuns in record patterns
-}
{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsPattern where

data Person = Person {name :: String, age :: Int}

greet :: Person -> String
greet Person {name} = "hello " ++ name
