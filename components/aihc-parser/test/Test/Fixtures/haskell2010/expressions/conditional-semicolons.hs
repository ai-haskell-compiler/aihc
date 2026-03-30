{- ORACLE_TEST
id: expr-s3-conditional-semicolons
category: expressions
expected: pass
-}
module ExprS306ConditionalSemicolons where
x n = if n > 0; then n; else 0
