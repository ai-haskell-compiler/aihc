{- ORACLE_TEST
id: modules-s5-import-as
category: modules
expected: pass
reason: parser now supports section 5 import aliases
-}
module S5ImportAs where
import Data.Maybe as M
x = M.fromMaybe 0 Nothing
