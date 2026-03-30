{- ORACLE_TEST
id: modules-s5-import-as-explicit-list
category: modules
expected: pass
reason: parser now supports section 5 import aliases with explicit lists
-}
module S5ImportAsExplicitList where
import Data.Maybe as M (fromMaybe)
x = M.fromMaybe 0 Nothing
