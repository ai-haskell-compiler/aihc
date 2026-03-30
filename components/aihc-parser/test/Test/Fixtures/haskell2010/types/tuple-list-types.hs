{- ORACLE_TEST
id: types-tuple-list
category: types
expected: pass
reason: parser now supports tuple type forms
-}
module T2 where
f :: [Int] -> (Int, Int)
f xs = (length xs, sum xs)
