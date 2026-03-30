{- ORACLE_TEST
id: types-let-signature
category: types
expected: pass
reason: parser now supports local let type signatures
-}
module T10 where
f :: Int -> Int
f n =
  let y :: Int
      y = n + 1
   in y
