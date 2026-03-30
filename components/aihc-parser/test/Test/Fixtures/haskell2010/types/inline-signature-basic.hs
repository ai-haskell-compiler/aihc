{- ORACLE_TEST
id: types-inline-signature-basic
category: types
expected: pass
reason: parser now supports inline type signatures
-}
module T7 where
value :: Int
value = (1 + 2 :: Int)
