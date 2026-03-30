{- ORACLE_TEST
id: decls-data-infix-backtick
category: declarations
expected: pass
reason: parser now supports backtick infix data constructors
-}
data A a b = a `Infix` b
