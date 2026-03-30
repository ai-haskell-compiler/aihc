{- ORACLE_TEST
id: types-tuple-type-constructor
category: types
expected: pass
reason: parser now supports tuple type constructors
-}
module T16 where
pair :: (,) a b -> (a, b)
pair x = x
