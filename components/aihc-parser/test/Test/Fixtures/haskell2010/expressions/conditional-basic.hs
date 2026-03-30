{- ORACLE_TEST
id: expr-s3-conditional-basic
category: expressions
expected: pass
reason: parser now supports basic conditional expressions
-}
module ExprS306ConditionalBasic where
x n = if n > 0 then n else 0
