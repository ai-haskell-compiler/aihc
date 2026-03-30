{- ORACLE_TEST
id: decls-operator-funlhs-infix-application
category: declarations
expected: pass
reason: parser supports parenthesized infix function heads with trailing arguments
-}
module M where
(g . h) x = g (h x)
