{- ORACLE_TEST
id: expr-s3-atoms-literal-all
category: expressions
expected: pass
reason: parser now handles all haskell2010 literal atom categories
-}
module ExprS302LiteralAll where
xDecimal = 42
xFloat = 3.14
xChar = 'a'
xString = "abc"
