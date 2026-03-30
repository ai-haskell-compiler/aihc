{- ORACLE_TEST
id: modules-import-as
category: modules
expected: pass
reason: parser now supports import aliases
-}
module IA where
import Data.Maybe as M
x = M.fromMaybe 0 Nothing
