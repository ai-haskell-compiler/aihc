{- ORACLE_TEST
id: expr-s3-section-bang
category: expressions
expected: pass
reason: parser supports bang operator sections in expressions
-}
module ExprSectionBang where
f = (! 3)
