{- ORACLE_TEST
id: expr-s3-pattern-constructor-application
category: expressions
expected: pass
-}
module ExprS317PatConstructorApplication where
data Box = Box Int
x b = case b of { Box n -> n }
