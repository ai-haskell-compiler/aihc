{- ORACLE_TEST pass -}
module ExprS314DoLetStmt where
x = do { let { n = 1 }; return n }