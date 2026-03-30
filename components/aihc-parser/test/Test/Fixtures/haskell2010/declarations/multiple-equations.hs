{- ORACLE_TEST
id: decls-multiple-equations
category: declarations
expected: pass
reason: parser now supports multiple equations
-}
module D2 where
f [] = 0
f (_:xs) = 1 + f xs
