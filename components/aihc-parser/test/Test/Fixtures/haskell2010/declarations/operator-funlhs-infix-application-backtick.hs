{- ORACLE_TEST
id: decls-operator-funlhs-infix-application-backtick
category: declarations
expected: pass
reason: parser supports parenthesized backtick infix function heads with trailing arguments
-}
module M where
(g `op` h) x = g (h x)
