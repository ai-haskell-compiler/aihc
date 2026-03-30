{- ORACLE_TEST
id: modules-s5-export-module-modid
category: modules
expected: pass
reason: parser now supports section 5 module export entries
-}
module S5ExportModuleModid (module Data.Maybe, x) where
import Data.Maybe
x = fromMaybe 0 Nothing
