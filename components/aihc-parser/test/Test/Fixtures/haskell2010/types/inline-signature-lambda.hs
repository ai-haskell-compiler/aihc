{- ORACLE_TEST
id: types-inline-signature-lambda
category: types
expected: pass
reason: parser now supports inline type signatures in lambdas
-}
module T9 where
idInt :: Int -> Int
idInt = (\x -> x :: Int)
