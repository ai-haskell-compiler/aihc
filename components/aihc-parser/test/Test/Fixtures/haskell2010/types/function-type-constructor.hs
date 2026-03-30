{- ORACLE_TEST
id: types-function-type-constructor
category: types
expected: pass
reason: parser now supports function type constructors in type positions
-}
module T14 where
apply :: (->) a b -> a -> b
apply f x = f x
