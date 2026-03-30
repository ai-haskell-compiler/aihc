{- ORACLE_TEST
id: decls-fixity
category: declarations
expected: pass
reason: parser now supports fixity declarations
-}
module D9 where
infixr 5 <++>
(<++>) :: [a] -> [a] -> [a]
(<++>) = (++ )
