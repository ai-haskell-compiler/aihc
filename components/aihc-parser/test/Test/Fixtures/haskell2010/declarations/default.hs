{- ORACLE_TEST
id: decls-default
category: declarations
expected: pass
reason: parser now supports default declarations
-}
module D10 where
default (Integer, Double)
