{- ORACLE_TEST
id: lexical-block-comments
category: lexical
expected: pass
reason: lexer now handles block comments
-}
module K1 where
{- block
   comment -}
x = 1
