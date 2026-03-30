{- ORACLE_TEST
id: modules-s5-import-hiding-empty
category: modules
expected: pass
reason: parser now supports section 5 empty hiding import lists
-}
module S5ImportHidingEmpty where
import Data.Maybe hiding ()
x = fromMaybe 0 Nothing
