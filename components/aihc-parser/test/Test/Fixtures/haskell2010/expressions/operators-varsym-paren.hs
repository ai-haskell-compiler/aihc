{- ORACLE_TEST
id: expr-s3-operators-varsym-paren
category: expressions
expected: pass
reason: parser now supports parenthesized symbolic operators as expressions
-}
module ExprS302VarsymParen where
x = (+) 1 2
