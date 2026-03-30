{- ORACLE_TEST
id: types-tuple-type-constructor-prefix
category: types
expected: pass
reason: parser now supports prefix tuple type constructor syntax like (,) a b
-}
module M where
f :: (,) a b -> Int
f = undefined
