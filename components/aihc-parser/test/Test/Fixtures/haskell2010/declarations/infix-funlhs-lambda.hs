{- ORACLE_TEST
id: decls-infix-funlhs-lambda
category: declarations
expected: pass
reason: parser now supports infix definitions with lambda bodies
-}
module InfixFunlhsLambda where
mb ?> x = mb >>= \b -> when b x
