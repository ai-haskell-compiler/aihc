{- ORACLE_TEST
id: modules-s5-import-qualified
category: modules
expected: pass
reason: parser now supports section 5 qualified imports
-}
module S5ImportQualified where
import qualified Data.Maybe
x = Data.Maybe.fromMaybe 0 Nothing
