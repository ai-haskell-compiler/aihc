{- ORACLE_TEST
id: decls-class-super-paren-multiple
category: declarations
expected: pass
reason: parser now supports parenthesized multi-superclass declarations without where
-}
module D30 where
class (Eq a, Show a) => C a
