{- ORACLE_TEST
id: types-guard-inline-signature
category: types
expected: pass
reason: parser now supports inline type signatures in guards
-}
module T11 where
choose :: Bool -> Bool
choose b
  | (not b :: Bool) = True
  | otherwise = False
