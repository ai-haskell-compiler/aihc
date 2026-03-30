{- ORACLE_TEST
id: decls-infix-funlhs-backtick
category: declarations
expected: pass
reason: parser preserves backtick infix function definitions in roundtrip
-}
module InfixFunlhsBacktick where
x `myOp` y = x + y
