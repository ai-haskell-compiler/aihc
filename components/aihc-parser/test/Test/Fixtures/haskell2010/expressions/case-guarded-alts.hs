{- ORACLE_TEST
id: expr-s3-case-guarded-alts
category: expressions
expected: pass
reason: parser now supports guarded case alternatives
-}
module ExprS313CaseGuardedAlts where
x n = case n of { m | m > 0 -> m; _ -> 0 }
