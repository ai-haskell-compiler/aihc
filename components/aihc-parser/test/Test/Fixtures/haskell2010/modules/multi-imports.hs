{- ORACLE_TEST
id: modules-multi-imports
category: modules
expected: pass
reason: parser supports multiple plain imports
-}
module IM where
import Data.Maybe
import Data.List
x = fromMaybe 0 (listToMaybe [1,2])
