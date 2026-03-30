{- ORACLE_TEST
id: layout-do-where-multi
category: layout
expected: pass
reason: where with multiple bindings at same column as do closes do block
-}
-- Test: where clause at same column as do statements closes do block (multiple bindings)
module DoWhereLayoutMulti where
testMulti a b = do
  x
  y
  where
    x = a
    y = b
