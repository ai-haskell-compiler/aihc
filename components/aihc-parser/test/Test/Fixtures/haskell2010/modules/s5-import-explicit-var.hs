{- ORACLE_TEST
id: modules-s5-import-explicit-var
category: modules
expected: pass
reason: parser now supports section 5 explicit variable imports
-}
module S5ImportExplicitVar where
import Data.Maybe (maybe)
x = maybe 0 id (Just 1)
