{- ORACLE_TEST
id: decls-infix-funlhs-guarded
category: declarations
expected: pass
reason: parser now supports infix definitions with guards
-}
module InfixFunlhsGuarded where
x <=> y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ
