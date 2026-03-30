{- ORACLE_TEST
id: named-puns-construct
category: expressions
expected: pass
reason: parser now supports NamedFieldPuns in record construction expressions
-}
{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsConstruct where

data Person = Person {name :: String, age :: Int}

mkPerson :: String -> Int -> Person
mkPerson name age = Person {name, age}
