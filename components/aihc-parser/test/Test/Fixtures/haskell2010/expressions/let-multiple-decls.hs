{- ORACLE_TEST
id: expr-s3-let-multiple-decls
category: expressions
expected: pass
reason: parser now supports multiple let declarations
-}
module ExprS312LetMultipleDecls where
x = let y = 1; z = 2 in y + z
