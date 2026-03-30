{- ORACLE_TEST
id: types-function-type-constructor-partial
category: types
expected: pass
reason: parser now supports partial function type constructors in type positions
-}
module T15 where
toMaybe :: (->) a (Maybe a)
toMaybe x = Just x
