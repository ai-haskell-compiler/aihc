{- ORACLE_TEST
id: parallel-list-comp-basic
category: expressions
expected: pass
reason: parser now supports basic parallel list comprehensions
-}
module ParallelListCompBasic where

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [ (x, y) | x <- xs | y <- ys ]
