{- ORACLE_TEST
id: decls-class-super-paren-single
category: declarations
expected: pass
reason: parser now supports parenthesized single-superclass declarations without where
-}
module D29 where
class (Eq a) => C a
