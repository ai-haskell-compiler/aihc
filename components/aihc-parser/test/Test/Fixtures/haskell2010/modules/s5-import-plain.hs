{- ORACLE_TEST
id: modules-s5-import-plain
category: modules
expected: pass
reason: parser supports plain section 5 imports
-}
module S5ImportPlain where
import Data.Maybe
x = fromMaybe 0 Nothing
