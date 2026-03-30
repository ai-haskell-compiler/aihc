{- ORACLE_TEST
id: modules-s5-module-body-impdecls-topdecls-braces
category: modules
expected: pass
reason: parser now supports section 5 braced module bodies with imports and declarations
-}
{ import Data.Maybe ; x = 1 }
