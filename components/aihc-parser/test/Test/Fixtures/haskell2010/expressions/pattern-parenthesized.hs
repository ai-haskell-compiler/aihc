{- ORACLE_TEST
id: expr-s3-pattern-parenthesized
category: expressions
expected: pass
reason: parser now supports parenthesized patterns
-}
module ExprS317PatParenthesized where
x t = case t of { (n) -> n }
