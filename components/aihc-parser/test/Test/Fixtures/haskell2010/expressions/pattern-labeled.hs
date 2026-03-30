{- ORACLE_TEST
id: expr-s3-pattern-labeled
category: expressions
expected: pass
-}
module ExprS317PatLabeled where
data R = R { a :: Int, b :: Int }
x r = case r of { R { a = n } -> n }
