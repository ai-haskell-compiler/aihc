{- ORACLE_TEST
id: decls-class-super-paren-empty
category: declarations
expected: pass
reason: parser now preserves explicit empty superclass context
-}
module D28 where
class () => C a
