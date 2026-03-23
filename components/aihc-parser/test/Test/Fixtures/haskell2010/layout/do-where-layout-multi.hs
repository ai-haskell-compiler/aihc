-- Test: where clause at same column as do statements closes do block (multiple bindings)
module DoWhereLayoutMulti where
testMulti a b = do
  x
  y
  where
    x = a
    y = b
