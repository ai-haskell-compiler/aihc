{- ORACLE_TEST
id: types-list-type-constructor
category: types
expected: pass
reason: parser now supports list type constructors
-}
module T12 where
idList :: [a] -> [a]
idList xs = xs
