{- ORACLE_TEST
id: modules-s5-import-explicit-tycon-cnames
category: modules
expected: pass
reason: parser now supports section 5 explicit type constructor imports with constructor names
-}
module S5ImportExplicitTyConCNames where
import Data.Maybe (Maybe(Nothing, Just))
x = Just 1
