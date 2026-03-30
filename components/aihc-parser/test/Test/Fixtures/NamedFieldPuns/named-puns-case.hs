{- ORACLE_TEST
id: named-puns-case
category: expressions
expected: pass
reason: parser now supports NamedFieldPuns in case pattern matching
-}
{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsCase where

data Point = Point {x :: Int, y :: Int}

sumPoint :: Point -> Int
sumPoint p =
  case p of
    Point {x, y} -> x + y
