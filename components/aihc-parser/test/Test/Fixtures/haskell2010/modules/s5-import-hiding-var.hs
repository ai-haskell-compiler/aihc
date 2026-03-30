{- ORACLE_TEST
id: modules-s5-import-hiding-var
category: modules
expected: pass
reason: parser now supports section 5 hiding import lists
-}
module S5ImportHidingVar where
import Data.Maybe hiding (maybe)
x = fromMaybe 0 Nothing
