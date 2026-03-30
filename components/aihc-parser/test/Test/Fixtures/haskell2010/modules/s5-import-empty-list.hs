{- ORACLE_TEST
id: modules-s5-import-empty-list
category: modules
expected: pass
reason: parser now supports empty section 5 import lists
-}
module S5ImportEmptyList where
import Data.Maybe ()
x = 1
