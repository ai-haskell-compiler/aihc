{- ORACLE_TEST
id: expr-s3-case-basic
category: expressions
expected: pass
reason: parser now supports basic case alternatives
-}
module ExprS313CaseBasic where
x n = case n of { 0 -> 1; _ -> 2 }
