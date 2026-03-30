{- ORACLE_TEST
id: decls-newtype
category: declarations
expected: pass
reason: parser now supports simple newtype declarations
-}
module D5 where
newtype Wrap a = Wrap a
