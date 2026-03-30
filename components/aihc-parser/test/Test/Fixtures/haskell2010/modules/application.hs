{- ORACLE_TEST
id: modules-application
category: modules
expected: pass
reason: parser now supports simple module application declarations
-}
module App where
f = g x
h = g (k 2)
