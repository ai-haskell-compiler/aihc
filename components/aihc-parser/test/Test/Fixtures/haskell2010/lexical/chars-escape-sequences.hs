{- ORACLE_TEST
id: lexical-chars-escape-sequences
category: lexical
expected: pass
reason: parser now handles char escape sequences
-}
module K2 where

x = '\SOH'
y = '\137'
z = '\CAN'
