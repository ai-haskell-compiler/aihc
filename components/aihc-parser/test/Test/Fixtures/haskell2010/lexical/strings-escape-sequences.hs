{- ORACLE_TEST
id: lexical-strings-escape-sequences
category: lexical
expected: pass
reason: parser now handles escaped string literal forms
-}
module K2 where

x = "\\SO\\&H"
y = "\\137\\&9"
z = "Here is a backslant \\\\ as well as \\137, \\\n         \\a numeric escape character, and \\^X, a control character."
