{- ORACLE_TEST
id: modules-s5-import-as-hiding
category: modules
expected: pass
reason: parser now supports section 5 import aliases with hiding lists
-}
module S5ImportAsHiding where
import Data.Maybe as M hiding (fromMaybe)
x = M.isNothing Nothing
