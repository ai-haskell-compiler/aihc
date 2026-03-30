{- ORACLE_TEST
id: decls-class-super-simple
category: declarations
expected: pass
reason: parser now supports superclass class declarations without where
-}
module D27 where
class Eq a => C a
