{- ORACLE_TEST
id: layout-do-where
category: layout
expected: pass
reason: where at same column as do statement closes do block
-}
-- Test: where clause at same column as do statement closes do block
module DoWhereLayout where
testDoWhereLayout a = do
  action
  where action = a
