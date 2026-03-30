{- ORACLE_TEST
id: expr-case-of
category: expressions
expected: pass
reason: parser now supports simple case-of expressions
-}
module X2 where
x m = case m of
  Nothing -> 0
  Just n -> n
