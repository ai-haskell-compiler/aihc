{- ORACLE_TEST
id: layout-where
category: layout
expected: pass
reason: parser now supports where layout forms
-}
module L2 where
x n = a + b
  where
    a = n + 1
    b = n + 2
