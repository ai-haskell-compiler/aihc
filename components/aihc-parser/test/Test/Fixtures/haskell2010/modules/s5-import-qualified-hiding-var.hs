{- ORACLE_TEST
id: modules-s5-import-qualified-hiding-var
category: modules
expected: pass
reason: parser now supports section 5 qualified hiding imports
-}
module S5ImportQualifiedHidingVar where
import qualified Data.Maybe hiding (fromMaybe)
x = Data.Maybe.isJust Nothing
