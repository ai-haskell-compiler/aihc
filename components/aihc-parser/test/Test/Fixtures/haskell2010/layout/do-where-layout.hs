-- Test: where clause at same column as do statement closes do block
module DoWhereLayout where
testDoWhereLayout a = do
  action
  where action = a
