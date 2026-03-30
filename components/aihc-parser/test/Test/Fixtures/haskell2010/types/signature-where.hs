{- ORACLE_TEST
id: types-signature-where
category: types
expected: pass
reason: parser now supports local type signatures in where bindings
-}
module T6 where
f :: Int -> Int
f n = helper n
  where
    helper :: Int -> Int
    helper x = x + 1
