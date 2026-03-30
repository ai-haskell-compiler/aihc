{- ORACLE_TEST
id: expr-s3-section-left
category: expressions
expected: pass
reason: parser now supports left operator sections
-}
module ExprS305SectionLeft where
x = map (1+) [1, 2, 3]
