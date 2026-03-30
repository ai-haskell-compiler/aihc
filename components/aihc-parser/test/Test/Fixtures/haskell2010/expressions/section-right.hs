{- ORACLE_TEST
id: expr-s3-section-right
category: expressions
expected: pass
reason: parser now supports right operator sections
-}
module ExprS305SectionRight where
x = map (+1) [1, 2, 3]
