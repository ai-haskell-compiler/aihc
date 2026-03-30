{- ORACLE_TEST
id: expr-s3-operators-backtick-conid
category: expressions
expected: pass
reason: parser now supports backtick constructor operators
-}
module ExprS302BacktickConid where
data Pair = Pair Int Int
x = 1 `Pair` 2
