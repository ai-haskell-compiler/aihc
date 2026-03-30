{- ORACLE_TEST
id: expr-s3-operators-backtick-varid
category: expressions
expected: pass
reason: parser now supports backtick variable operators
-}
module ExprS302BacktickVarid where
x = 5 `mod` 2
