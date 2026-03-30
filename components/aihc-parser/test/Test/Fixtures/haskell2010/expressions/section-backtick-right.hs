{- ORACLE_TEST
id: expr-s3-section-backtick-right
category: expressions
expected: pass
reason: parser now supports right backtick operator sections
-}
module ExprS305SectionBacktickRight where
x = map (2 `div`) [2, 4, 6]
