{- ORACLE_TEST
id: modules-s5-import-qualified-as
category: modules
expected: pass
reason: parser now supports section 5 qualified imports with aliases
-}
module S5ImportQualifiedAs where
import qualified Data.Maybe as M
x = M.fromMaybe 0 Nothing
