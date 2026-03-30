{- ORACLE_TEST
id: types-multi-vars-signature
category: types
expected: pass
reason: parser now supports multi-variable top-level signatures
-}
module T4 where
f, g :: Int -> Int
f x = x + 1
g x = x - 1
