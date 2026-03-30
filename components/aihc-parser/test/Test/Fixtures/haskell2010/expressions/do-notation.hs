{- ORACLE_TEST
id: expr-do-notation
category: expressions
expected: pass
reason: parser now handles do-notation with layout and explicit braces
-}
module X7 where
x = do
  a <- Just 1
  b <- Just 2
  return (a + b)
