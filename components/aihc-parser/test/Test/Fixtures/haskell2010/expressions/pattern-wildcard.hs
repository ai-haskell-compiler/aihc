{- ORACLE_TEST
id: expr-s3-pattern-wildcard
category: expressions
expected: pass
reason: parser now supports wildcard patterns
-}
module ExprS317PatWildcard where
x t = case t of { (_, _) -> 1 }
