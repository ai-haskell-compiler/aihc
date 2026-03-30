{- ORACLE_TEST
id: expr-list-comprehension
category: expressions
expected: pass
reason: parser now supports list comprehensions with generators and guards
-}
module X9 where
x = [n * 2 | n <- [1..10], odd n]
