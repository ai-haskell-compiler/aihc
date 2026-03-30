{- ORACLE_TEST
id: types-inline-signature-context
category: types
expected: pass
reason: parser now supports inline type signatures with context
-}
module T8 where
inc :: Int -> Int
inc = ((+ 1) :: Num a => a -> a)
