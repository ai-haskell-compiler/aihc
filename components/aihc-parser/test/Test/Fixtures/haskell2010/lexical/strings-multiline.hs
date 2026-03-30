{- ORACLE_TEST
id: lexical-strings-multiline
category: lexical
expected: pass
reason: parser now handles multiline string gaps
-}
module K2 where

x = "This string spans\
     \ multiple lines."
