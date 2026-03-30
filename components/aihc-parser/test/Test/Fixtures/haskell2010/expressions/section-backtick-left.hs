{- ORACLE_TEST
id: expr-s3-section-backtick-left
category: expressions
expected: pass
reason: parser now supports left backtick operator sections
-}
module ExprS305SectionBacktickLeft where
x = map (`div` 2) [2, 4, 6]
