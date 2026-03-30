{- ORACLE_TEST
id: modules-s5-import-explicit-tycon
category: modules
expected: pass
reason: parser now supports section 5 explicit type constructor imports
-}
module S5ImportExplicitTyCon where
import Data.Maybe (Maybe)
x = Just 1
