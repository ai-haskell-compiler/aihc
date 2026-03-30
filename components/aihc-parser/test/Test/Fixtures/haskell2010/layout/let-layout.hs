{- ORACLE_TEST
id: layout-let
category: layout
expected: pass
reason: parser now supports let layout forms
-}
module L1 where
x =
  let a = 1
      b = 2
  in a + b
