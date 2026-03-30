{- ORACLE_TEST
id: modules-s5-import-explicit-tycon-all
category: modules
expected: pass
reason: parser now supports section 5 explicit type constructor imports with wildcard
-}
module S5ImportExplicitTyConAll where
import Data.Maybe (Maybe(..))
x = Just 1
